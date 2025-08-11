/**
 * mapbox-pmtiles v1.1.0 - Optimized Version with Lifecycle Management
 * Original source: https://github.com/am2222/mapbox-pmtiles by Majid Hojati
 * License: MIT
 *
 * This is an optimized version of the mapbox-pmtiles library that provides
 * better performance for large datasets through:
 * - Configurable resource management
 * - Instance-scoped worker pools and caches
 * - Proper lifecycle management and cleanup
 * - Reference counting for shared resources
 * - Enhanced error handling and timeouts
 * - Memory optimization with size-based LRU caches
 *
 * Last updated: 2025
 */

// Note: This version assumes mapboxgl and pmtiles are already loaded as globals
(function (global) {
  "use strict";

  // Helper functions
  var __pow = Math.pow;
  var __async = (__this, __arguments, generator) => {
    return new Promise((resolve, reject) => {
      var fulfilled = (value) => {
        try {
          step(generator.next(value));
        } catch (e) {
          reject(e);
        }
      };
      var rejected = (value) => {
        try {
          step(generator.throw(value));
        } catch (e) {
          reject(e);
        }
      };
      var step = (x2) =>
        x2.done
          ? resolve(x2.value)
          : Promise.resolve(x2.value).then(fulfilled, rejected);
      step((generator = generator.apply(__this, __arguments)).next());
    });
  };

  // Check dependencies
  if (typeof mapboxgl === "undefined") {
    console.error("mapbox-pmtiles: Mapbox GL JS is not loaded");
    return;
  }
  if (typeof pmtiles === "undefined") {
    console.error("mapbox-pmtiles: PMTiles library is not loaded");
    return;
  }

  const VectorTileSourceImpl = mapboxgl.Style.getSourceType("vector");
  const SOURCE_TYPE = "pmtile-source";

  // Global shared resources with reference counting
  const GLOBAL_SHARED_RESOURCES = {
    // Protocol cache - expensive to duplicate, shared with reference counting
    protocolCache: new Map(), // url -> { protocol, instance, refCount }

    // Metadata cache - small and shareable
    metadataCache: new Map(), // cacheKey -> data

    // Pre-calculated world sizes for common zoom levels (static, no cleanup needed)
    worldSizeCache: new Array(25).fill(null).map((_, z) => Math.pow(2, z)),

    // Global cleanup registry
    activeManagers: new Set(),

    // Debug/development features
    debug: false,
    performanceMetrics: new Map(),
  };

  /**
   * Default configuration options
   */
  const DEFAULT_OPTIONS = {
    workerPoolSize: 4,
    tileCacheSize: 1000,
    tileCacheMaxMemoryMB: 100,
    metadataCacheSize: 100,
    enableSharedProtocols: true,
    requestTimeoutMs: 30000,
    enableDebugLogging: false,
    enablePerformanceMetrics: false,
  };

  /**
   * LRU Cache implementation with size-based eviction
   */
  class LRUCache {
    constructor(maxSize, maxMemoryBytes = Infinity) {
      this.maxSize = maxSize;
      this.maxMemoryBytes = maxMemoryBytes;
      this.cache = new Map();
      this.currentMemoryBytes = 0;
    }

    get(key) {
      if (this.cache.has(key)) {
        // Move to end (most recently used)
        const value = this.cache.get(key);
        this.cache.delete(key);
        this.cache.set(key, value);
        return value.data;
      }
      return undefined;
    }

    set(key, data, estimatedSize = 0) {
      // Remove if exists
      if (this.cache.has(key)) {
        const existing = this.cache.get(key);
        this.currentMemoryBytes -= existing.size;
        this.cache.delete(key);
      }

      // Evict old entries if necessary
      while (
        this.cache.size >= this.maxSize ||
        this.currentMemoryBytes + estimatedSize > this.maxMemoryBytes
      ) {
        const firstKey = this.cache.keys().next().value;
        if (!firstKey) break;

        const firstValue = this.cache.get(firstKey);
        this.currentMemoryBytes -= firstValue.size;
        this.cache.delete(firstKey);
      }

      // Add new entry
      this.cache.set(key, { data, size: estimatedSize });
      this.currentMemoryBytes += estimatedSize;
    }

    has(key) {
      return this.cache.has(key);
    }

    delete(key) {
      if (this.cache.has(key)) {
        const value = this.cache.get(key);
        this.currentMemoryBytes -= value.size;
        this.cache.delete(key);
        return true;
      }
      return false;
    }

    clear() {
      this.cache.clear();
      this.currentMemoryBytes = 0;
    }

    get size() {
      return this.cache.size;
    }

    getMemoryUsage() {
      return {
        entries: this.cache.size,
        memoryBytes: this.currentMemoryBytes,
        memoryMB: this.currentMemoryBytes / (1024 * 1024),
      };
    }
  }

  /**
   * Resource Manager - handles per-instance resources and lifecycle
   */
  class PMTilesResourceManager {
    constructor(options = {}) {
      this.config = { ...DEFAULT_OPTIONS, ...options };
      this.destroyed = false;
      this.paused = false;
      this.dispatcher = null;

      // Instance-scoped resources
      this.workerPool = [];
      this.workerPoolIndex = 0;
      this.tileCache = new LRUCache(
        this.config.tileCacheSize,
        this.config.tileCacheMaxMemoryMB * 1024 * 1024,
      );
      this.pendingRequests = new Map();
      this.activeRequests = new Set();

      // Performance tracking
      this.metrics = {
        tilesLoaded: 0,
        cacheHits: 0,
        cacheMisses: 0,
        memoryPeakMB: 0,
        averageLoadTimeMs: 0,
      };

      // Register for global cleanup
      GLOBAL_SHARED_RESOURCES.activeManagers.add(this);

      if (this.config.enableDebugLogging) {
        console.log("[PMTiles] Resource manager created", this.config);
      }
    }

    /**
     * Initialize worker pool
     */
    initializeWorkerPool(dispatcher) {
      if (this.destroyed) return;

      // Store dispatcher reference
      if (dispatcher) {
        this.dispatcher = dispatcher;
      }

      if (this.workerPool.length === 0 && this.dispatcher) {
        for (let i = 0; i < this.config.workerPoolSize; i++) {
          try {
            this.workerPool.push(this.dispatcher.getActor());
          } catch (error) {
            console.warn("[PMTiles] Failed to create worker:", error);
          }
        }

        if (this.config.enableDebugLogging) {
          console.log(
            `[PMTiles] Initialized worker pool with ${this.workerPool.length} workers`,
          );
        }
      }
    }

    /**
     * Get next worker from pool (round-robin)
     */
    getWorkerFromPool() {
      if (this.destroyed) {
        return null;
      }

      // Try to initialize workers if not done yet
      if (this.workerPool.length === 0 && this.dispatcher) {
        this.initializeWorkerPool(this.dispatcher);
      }

      if (this.workerPool.length === 0) {
        if (this.config.enableDebugLogging) {
          console.warn(
            "[PMTiles] Worker pool is empty, dispatcher available:",
            !!this.dispatcher,
          );
        }
        return null;
      }

      const worker = this.workerPool[this.workerPoolIndex];
      this.workerPoolIndex =
        (this.workerPoolIndex + 1) % this.workerPool.length;
      return worker;
    }

    /**
     * Get or create protocol instance with reference counting
     */
    getProtocol(url) {
      if (this.destroyed) return null;

      if (!this.config.enableSharedProtocols) {
        // Create instance-specific protocol
        const protocol = new pmtiles.Protocol();
        const instance = new pmtiles.PMTiles(url);
        protocol.add(instance);
        return { protocol, instance };
      }

      // Use shared protocol with reference counting
      if (!GLOBAL_SHARED_RESOURCES.protocolCache.has(url)) {
        const protocol = new pmtiles.Protocol();
        const instance = new pmtiles.PMTiles(url);
        protocol.add(instance);
        GLOBAL_SHARED_RESOURCES.protocolCache.set(url, {
          protocol,
          instance,
          refCount: 0,
        });
      }

      const cached = GLOBAL_SHARED_RESOURCES.protocolCache.get(url);
      cached.refCount++;
      return cached;
    }

    /**
     * Release protocol reference
     */
    releaseProtocol(url) {
      if (!this.config.enableSharedProtocols) return;

      const cached = GLOBAL_SHARED_RESOURCES.protocolCache.get(url);
      if (cached) {
        cached.refCount--;
        if (cached.refCount <= 0) {
          GLOBAL_SHARED_RESOURCES.protocolCache.delete(url);

          if (this.config.enableDebugLogging) {
            console.log(`[PMTiles] Released protocol for ${url}`);
          }
        }
      }
    }

    /**
     * Cache key for tiles
     */
    getTileCacheKey(url, z, x, y) {
      return `${url}:${z}:${x}:${y}`;
    }

    /**
     * Add tile to cache with size estimation
     */
    addToTileCache(key, data) {
      if (this.destroyed) return;

      let estimatedSize = 0;
      if (data instanceof ImageBitmap) {
        // Rough estimation: width * height * 4 bytes per pixel
        estimatedSize = data.width * data.height * 4;
      } else if (data && data.byteLength) {
        estimatedSize = data.byteLength;
      } else {
        estimatedSize = 10000; // Default estimate
      }

      this.tileCache.set(key, data, estimatedSize);

      // Update peak memory usage
      const memoryUsage = this.tileCache.getMemoryUsage();
      this.metrics.memoryPeakMB = Math.max(
        this.metrics.memoryPeakMB,
        memoryUsage.memoryMB,
      );
    }

    /**
     * Get cached metadata
     */
    getCachedMetadata(cacheKey) {
      return GLOBAL_SHARED_RESOURCES.metadataCache.get(cacheKey);
    }

    /**
     * Set cached metadata
     */
    setCachedMetadata(cacheKey, data) {
      GLOBAL_SHARED_RESOURCES.metadataCache.set(cacheKey, data);
    }

    /**
     * Pause all operations
     */
    pause() {
      this.paused = true;

      // Cancel all pending requests
      for (const [key, request] of this.pendingRequests) {
        if (request.cancel) {
          request.cancel();
        }
      }
      this.pendingRequests.clear();

      if (this.config.enableDebugLogging) {
        console.log("[PMTiles] Resource manager paused");
      }
    }

    /**
     * Resume operations
     */
    resume() {
      this.paused = false;

      if (this.config.enableDebugLogging) {
        console.log("[PMTiles] Resource manager resumed");
      }
    }

    /**
     * Get performance metrics
     */
    getMetrics() {
      return {
        ...this.metrics,
        tileCache: this.tileCache.getMemoryUsage(),
        workerPoolSize: this.workerPool.length,
        pendingRequests: this.pendingRequests.size,
        isPaused: this.paused,
        isDestroyed: this.destroyed,
      };
    }

    /**
     * Destroy and cleanup all resources
     */
    destroy() {
      if (this.destroyed) return;

      this.destroyed = true;
      this.paused = true;

      // Cancel all pending requests
      for (const [key, request] of this.pendingRequests) {
        if (request.cancel) {
          request.cancel();
        }
      }
      this.pendingRequests.clear();

      // Clear caches
      this.tileCache.clear();

      // Clear worker pool references
      this.workerPool.length = 0;

      // Remove from global registry
      GLOBAL_SHARED_RESOURCES.activeManagers.delete(this);

      if (this.config.enableDebugLogging) {
        console.log("[PMTiles] Resource manager destroyed", this.getMetrics());
      }
    }
  }

  /**
   * Global cleanup function
   */
  const cleanup = () => {
    for (const manager of GLOBAL_SHARED_RESOURCES.activeManagers) {
      manager.destroy();
    }
    GLOBAL_SHARED_RESOURCES.protocolCache.clear();
    GLOBAL_SHARED_RESOURCES.metadataCache.clear();
  };

  // Register global cleanup
  if (typeof window !== "undefined") {
    window.addEventListener("beforeunload", cleanup);
  }

  const extend = (dest, ...sources) => {
    for (const src of sources) {
      for (const k in src) {
        dest[k] = src[k];
      }
    }
    return dest;
  };

  const mercatorXFromLng = (lng) => {
    return (180 + lng) / 360;
  };

  const mercatorYFromLat = (lat) => {
    return (
      (180 -
        (180 / Math.PI) *
          Math.log(Math.tan(Math.PI / 4 + (lat * Math.PI) / 360))) /
      360
    );
  };

  class TileBounds {
    constructor(bounds, minzoom, maxzoom) {
      this.bounds = mapboxgl.LngLatBounds.convert(this.validateBounds(bounds));
      this.minzoom = minzoom || 0;
      this.maxzoom = maxzoom || 24;

      // Pre-calculate mercator bounds
      this._mercatorBounds = {
        west: mercatorXFromLng(this.bounds.getWest()),
        north: mercatorYFromLat(this.bounds.getNorth()),
        east: mercatorXFromLng(this.bounds.getEast()),
        south: mercatorYFromLat(this.bounds.getSouth()),
      };
    }

    validateBounds(bounds) {
      if (!Array.isArray(bounds) || bounds.length !== 4)
        return [-180, -90, 180, 90];
      return [
        Math.max(-180, bounds[0]),
        Math.max(-90, bounds[1]),
        Math.min(180, bounds[2]),
        Math.min(90, bounds[3]),
      ];
    }

    contains(tileID) {
      // Use pre-calculated world size
      const worldSize =
        GLOBAL_SHARED_RESOURCES.worldSizeCache[tileID.z] ||
        Math.pow(2, tileID.z);

      // Use pre-calculated mercator bounds
      const level = {
        minX: Math.floor(this._mercatorBounds.west * worldSize),
        minY: Math.floor(this._mercatorBounds.north * worldSize),
        maxX: Math.ceil(this._mercatorBounds.east * worldSize),
        maxY: Math.ceil(this._mercatorBounds.south * worldSize),
      };

      const hit =
        tileID.x >= level.minX &&
        tileID.x < level.maxX &&
        tileID.y >= level.minY &&
        tileID.y < level.maxY;
      return hit;
    }
  }

  class Event {
    constructor(type, data = {}) {
      extend(this, data);
      this.type = type;
    }
  }

  class ErrorEvent extends Event {
    constructor(error, data = {}) {
      super("error", extend({ error }, data));
    }
  }

  /**
   * Enhanced PMTiles Source with lifecycle management
   */
  class PmTilesSource extends VectorTileSourceImpl {
    constructor(id, options, _dispatcher, _eventedParent) {
      super(...[id, options, _dispatcher, _eventedParent]);

      // Extract PMTiles-specific options
      const pmtilesOptions = {
        workerPoolSize: options.workerPoolSize,
        tileCacheSize: options.tileCacheSize,
        tileCacheMaxMemoryMB: options.tileCacheMaxMemoryMB,
        metadataCacheSize: options.metadataCacheSize,
        enableSharedProtocols: options.enableSharedProtocols,
        requestTimeoutMs: options.requestTimeoutMs,
        enableDebugLogging: options.enableDebugLogging,
        enablePerformanceMetrics: options.enablePerformanceMetrics,
      };

      // Initialize resource manager
      this.resourceManager = new PMTilesResourceManager(pmtilesOptions);

      // Standard source properties
      this.scheme = "xyz";
      this.roundZoom = true;
      this.type = "vector";
      this.dispatcher = _dispatcher;
      this.reparseOverscaled = true;
      this._loaded = false;
      this._dataType = "vector";
      this.id = id;
      this._implementation = options;

      // Initialize worker pool
      this.resourceManager.initializeWorkerPool(_dispatcher);

      if (!this._implementation) {
        this.fire(
          new ErrorEvent(
            new Error(`Missing options for ${this.id} ${SOURCE_TYPE} source`),
          ),
        );
        return;
      }

      const { url } = options;
      this.url = url;
      this.tileSize = 512;

      // Get protocol instance
      this.protocolInfo = this.resourceManager.getProtocol(url);
      if (!this.protocolInfo) {
        this.fire(
          new ErrorEvent(new Error(`Failed to create protocol for ${url}`)),
        );
        return;
      }

      this._protocol = this.protocolInfo.protocol;
      this._instance = this.protocolInfo.instance;
      this.tiles = [`pmtiles://${url}/{z}/{x}/{y}`];
    }

    static async getMetadata(url) {
      // Check cache first
      const cacheKey = `${url}:metadata`;
      const cached = GLOBAL_SHARED_RESOURCES.metadataCache.get(cacheKey);
      if (cached) {
        return cached;
      }

      const instance = new pmtiles.PMTiles(url);
      const metadata = await instance.getMetadata();
      GLOBAL_SHARED_RESOURCES.metadataCache.set(cacheKey, metadata);
      return metadata;
    }

    static async getHeader(url) {
      // Check cache first
      const cacheKey = `${url}:header`;
      const cached = GLOBAL_SHARED_RESOURCES.metadataCache.get(cacheKey);
      if (cached) {
        return cached;
      }

      const instance = new pmtiles.PMTiles(url);
      const header = await instance.getHeader();
      GLOBAL_SHARED_RESOURCES.metadataCache.set(cacheKey, header);
      return header;
    }

    getExtent() {
      if (!this.header)
        return [
          [-180, -90],
          [180, 90],
        ];
      const { minLon, minLat, maxLon, maxLat } = this.header;
      return [minLon, minLat, maxLon, maxLat];
    }

    hasTile(tileID) {
      return !this.tileBounds || this.tileBounds.contains(tileID.canonical);
    }

    fixTile(tile) {
      if (this.resourceManager.destroyed) return;

      if (!tile.destroy) {
        tile.destroy = () => {};
      }
      if (!tile.abort) {
        tile.abort = () => {
          tile.aborted = true;
          if (tile.request && tile.request.cancel) {
            tile.request.cancel();
          }
        };
      }
    }

    /**
     * Pause tile loading
     */
    pause() {
      this.resourceManager.pause();
    }

    /**
     * Resume tile loading
     */
    resume() {
      this.resourceManager.resume();
    }

    /**
     * Get performance metrics
     */
    getMetrics() {
      return this.resourceManager.getMetrics();
    }

    /**
     * Destroy source and cleanup resources
     */
    destroy() {
      if (this.protocolInfo && this.url) {
        this.resourceManager.releaseProtocol(this.url);
      }

      this.resourceManager.destroy();
      this._loaded = false;
    }

    async load(callback) {
      if (this.resourceManager.destroyed) {
        const error = new Error("Source has been destroyed");
        this.fire(new ErrorEvent(error));
        if (callback) callback(error);
        return;
      }

      this._loaded = false;
      this.fire(new Event("dataloading", { dataType: "source" }));

      // Check metadata cache first
      const headerKey = `${this.url}:header`;
      const metadataKey = `${this.url}:metadata`;

      let header, tileJSON;

      const cachedHeader = this.resourceManager.getCachedMetadata(headerKey);
      const cachedMetadata =
        this.resourceManager.getCachedMetadata(metadataKey);

      if (cachedHeader && cachedMetadata) {
        header = cachedHeader;
        tileJSON = cachedMetadata;
      } else {
        try {
          // Load and cache
          [header, tileJSON] = await Promise.all([
            this._instance.getHeader(),
            this._instance.getMetadata(),
          ]);
          this.resourceManager.setCachedMetadata(headerKey, header);
          this.resourceManager.setCachedMetadata(metadataKey, tileJSON);
        } catch (error) {
          this.fire(new ErrorEvent(error));
          if (callback) callback(error);
          return;
        }
      }

      try {
        extend(this, tileJSON);
        this.header = header;
        const { tileType, minZoom, maxZoom, minLon, minLat, maxLon, maxLat } =
          header;
        const requiredVariables = [
          minZoom,
          maxZoom,
          minLon,
          minLat,
          maxLon,
          maxLat,
        ];

        if (
          !requiredVariables.includes(void 0) &&
          !requiredVariables.includes(null)
        ) {
          this.tileBounds = new TileBounds(
            [minLon, minLat, maxLon, maxLat],
            minZoom,
            maxZoom,
          );
          this.minzoom = minZoom;
          this.maxzoom = maxZoom;
        }

        if (this.maxzoom == void 0) {
          console.warn(
            "The maxzoom parameter is not defined in the source json. This can cause memory leak. So make sure to define maxzoom in the layer",
          );
        }

        this.minzoom = Number.parseInt(this.minzoom.toString()) || 0;
        this.maxzoom = Number.parseInt(this.maxzoom.toString()) || 0;
        this._loaded = true;
        this.tileType = tileType;

        switch (tileType) {
          case pmtiles.TileType.Png:
            this.contentType = "image/png";
            break;
          case pmtiles.TileType.Jpeg:
            this.contentType = "image/jpeg";
            break;
          case pmtiles.TileType.Webp:
            this.contentType = "image/webp";
            break;
          case pmtiles.TileType.Avif:
            this.contentType = "image/avif";
            break;
          case pmtiles.TileType.Mvt:
            this.contentType = "application/vnd.mapbox-vector-tile";
            break;
        }

        if (
          [pmtiles.TileType.Jpeg, pmtiles.TileType.Png].includes(this.tileType)
        ) {
          this.loadTile = this.loadRasterTile;
          this.type = "raster";
        } else if (this.tileType === pmtiles.TileType.Mvt) {
          this.loadTile = this.loadVectorTile;
          this.type = "vector";
        } else {
          this.fire(new ErrorEvent(new Error("Unsupported Tile Type")));
        }

        this.fire(
          new Event("data", { dataType: "source", sourceDataType: "metadata" }),
        );
        this.fire(
          new Event("data", { dataType: "source", sourceDataType: "content" }),
        );
      } catch (err2) {
        this.fire(new ErrorEvent(err2));
        if (callback) callback(err2);
      }
    }

    loaded() {
      return this._loaded && !this.resourceManager.destroyed;
    }

    loadVectorTile(tile, callback) {
      if (this.resourceManager.destroyed || this.resourceManager.paused) {
        return callback(null);
      }

      const startTime = Date.now();
      var _a2, _b2, _c;

      const done = (err2, data) => {
        var _a3, _b3;
        delete tile.request;

        // Update metrics
        this.resourceManager.metrics.tilesLoaded++;
        const loadTime = Date.now() - startTime;
        this.resourceManager.metrics.averageLoadTimeMs =
          (this.resourceManager.metrics.averageLoadTimeMs + loadTime) / 2;

        if (tile.aborted) return callback(null);

        // Handle abort errors gracefully
        if (err2 && err2.name === "AbortError") {
          return callback(null);
        }

        if (err2 && err2.status !== 404) {
          return callback(err2);
        }

        if (data && data.resourceTiming)
          tile.resourceTiming = data.resourceTiming;
        if (
          ((_a3 = this.map) == null ? void 0 : _a3._refreshExpiredTiles) &&
          data
        )
          tile.setExpiryData(data);
        tile.loadVectorData(
          data,
          (_b3 = this.map) == null ? void 0 : _b3.painter,
        );
        callback(null);

        if (tile.reloadCallback) {
          this.loadVectorTile(tile, tile.reloadCallback);
          tile.reloadCallback = null;
        }
      };

      const url =
        (_a2 = this.map) == null
          ? void 0
          : _a2._requestManager.normalizeTileURL(
              tile.tileID.canonical.url(this.tiles, this.scheme),
            );
      const request =
        (_b2 = this.map) == null
          ? void 0
          : _b2._requestManager.transformRequest(url, "Tile");

      const params = {
        request,
        data: {},
        uid: tile.uid,
        tileID: tile.tileID,
        tileZoom: tile.tileZoom,
        zoom: tile.tileID.overscaledZ,
        tileSize: this.tileSize * tile.tileID.overscaleFactor(),
        type: "vector",
        source: this.id,
        scope: this.scope,
        showCollisionBoxes:
          (_c = this.map) == null ? void 0 : _c.showCollisionBoxes,
        promoteId: this.promoteId,
        isSymbolTile: tile.isSymbolTile,
        extraShadowCaster: tile.isExtraShadowCaster,
      };

      const afterLoad = (error, data, cacheControl, expires) => {
        if (error || !data) {
          // Handle abort errors gracefully
          if (error && (error.name === "AbortError" || error.code === 20)) {
            return done.call(this, null);
          }
          done.call(this, error);
          return;
        }

        params.data = {
          cacheControl,
          expires,
          rawData: data,
        };

        if (this.map._refreshExpiredTiles)
          tile.setExpiryData({ cacheControl, expires });
        if (tile.actor)
          tile.actor.send("loadTile", params, done.bind(this), void 0, true);
      };

      this.fixTile(tile);
      if (!tile.actor || tile.state === "expired") {
        // Use shared worker pool
        tile.actor = this.resourceManager.getWorkerFromPool();

        // Fallback to dispatcher if worker pool failed
        if (!tile.actor && this.dispatcher) {
          try {
            tile.actor = this.dispatcher.getActor();
          } catch (error) {
            console.warn("[PMTiles] Failed to get fallback worker:", error);
            return callback(new Error("No workers available"));
          }
        }

        if (!tile.actor) {
          return callback(new Error("No workers available"));
        }

        // Create request with timeout
        const requestPromise = this._protocol.tile({ ...request }, afterLoad);

        // Add timeout if configured
        if (this.resourceManager.config.requestTimeoutMs > 0) {
          const timeoutId = setTimeout(() => {
            if (tile.request && tile.request.cancel) {
              tile.request.cancel();
            }
            done.call(this, new Error("Request timeout"));
          }, this.resourceManager.config.requestTimeoutMs);

          const originalCancel = requestPromise.cancel;
          requestPromise.cancel = () => {
            clearTimeout(timeoutId);
            if (originalCancel) originalCancel();
          };
        }

        tile.request = requestPromise;
      } else if (tile.state === "loading") {
        tile.reloadCallback = callback;
      } else {
        tile.request = this._protocol.tile({ ...tile, url }, afterLoad);
      }
    }

    loadRasterTileData(tile, data) {
      if (this.resourceManager.destroyed) return;
      tile.setTexture(data, this.map.painter);
    }

    loadRasterTile(tile, callback) {
      if (this.resourceManager.destroyed || this.resourceManager.paused) {
        return callback(null);
      }

      var _a2, _b2;

      // Check tile cache first
      const cacheKey = this.resourceManager.getTileCacheKey(
        this.url,
        tile.tileID.canonical.z,
        tile.tileID.canonical.x,
        tile.tileID.canonical.y,
      );

      if (this.resourceManager.tileCache.has(cacheKey)) {
        this.resourceManager.metrics.cacheHits++;
        const cachedData = this.resourceManager.tileCache.get(cacheKey);
        this.loadRasterTileData(tile, cachedData);
        tile.state = "loaded";
        return callback(null);
      }

      this.resourceManager.metrics.cacheMisses++;

      const done = ({ data, cacheControl, expires }) => {
        delete tile.request;
        if (tile.aborted) return callback(null);
        if (data === null || data === void 0) {
          const emptyImage = {
            width: this.tileSize,
            height: this.tileSize,
            data: null,
          };
          this.loadRasterTileData(tile, emptyImage);
          tile.state = "loaded";
          return callback(null);
        }

        if (data && data.resourceTiming)
          tile.resourceTiming = data.resourceTiming;
        if (this.map._refreshExpiredTiles)
          tile.setExpiryData({ cacheControl, expires });

        // Optimized raster tile loading - try direct ArrayBuffer first
        const arrayBuffer = data.buffer || data;
        window
          .createImageBitmap(arrayBuffer)
          .then((imageBitmap) => {
            // Cache the decoded image
            this.resourceManager.addToTileCache(cacheKey, imageBitmap);

            this.loadRasterTileData(tile, imageBitmap);
            tile.state = "loaded";
            callback(null);
          })
          .catch((error) => {
            // Fallback to blob method
            const blob = new window.Blob([new Uint8Array(data)], {
              type: this.contentType,
            });
            window
              .createImageBitmap(blob)
              .then((imageBitmap) => {
                this.resourceManager.addToTileCache(cacheKey, imageBitmap);
                this.loadRasterTileData(tile, imageBitmap);
                tile.state = "loaded";
                callback(null);
              })
              .catch((error) => {
                tile.state = "errored";
                return callback(
                  new Error(`Can't decode image for ${this.id}: ${error}`),
                );
              });
          });
      };

      const url =
        (_a2 = this.map) == null
          ? void 0
          : _a2._requestManager.normalizeTileURL(
              tile.tileID.canonical.url(this.tiles, this.scheme),
            );
      const request =
        (_b2 = this.map) == null
          ? void 0
          : _b2._requestManager.transformRequest(url, "Tile");

      this.fixTile(tile);
      const controller = new AbortController();

      // Add timeout if configured
      let timeoutId;
      if (this.resourceManager.config.requestTimeoutMs > 0) {
        timeoutId = setTimeout(() => {
          controller.abort();
        }, this.resourceManager.config.requestTimeoutMs);
      }

      tile.request = {
        cancel: () => {
          if (timeoutId) clearTimeout(timeoutId);
          controller.abort();
        },
      };

      this._protocol
        .tile(request, controller)
        .then(done.bind(this))
        .catch((error) => {
          if (timeoutId) clearTimeout(timeoutId);

          // Handle abort errors gracefully
          if (error.name === "AbortError" || error.code === 20) {
            delete tile.request;
            return callback(null);
          }
          tile.state = "errored";
          callback(error);
        });
    }
  }

  PmTilesSource.SOURCE_TYPE = SOURCE_TYPE;

  // Expose cleanup function
  PmTilesSource.cleanup = cleanup;

  // Export to global scope
  global.MapboxPmTilesSource = PmTilesSource;
  global.PMTILES_SOURCE_TYPE = SOURCE_TYPE;
  global.PMTilesResourceManager = PMTilesResourceManager;
})(typeof window !== "undefined" ? window : this);
