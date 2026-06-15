/**
 * mapbox-pmtiles v1.0.53
 * Original source: https://github.com/am2222/mapbox-pmtiles
 * License: MIT
 *
 * This is a vendored copy of the mapbox-pmtiles library that provides
 * PMTiles support for Mapbox GL JS by implementing a custom source type.
 *
 * Last updated: 2024
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
      const worldSize = Math.pow(2, tileID.z);
      const level = {
        minX: Math.floor(mercatorXFromLng(this.bounds.getWest()) * worldSize),
        minY: Math.floor(mercatorYFromLat(this.bounds.getNorth()) * worldSize),
        maxX: Math.ceil(mercatorXFromLng(this.bounds.getEast()) * worldSize),
        maxY: Math.ceil(mercatorYFromLat(this.bounds.getSouth()) * worldSize),
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

  class PmTilesSource extends VectorTileSourceImpl {
    constructor(id, options, _dispatcher, _eventedParent) {
      super(...[id, options, _dispatcher, _eventedParent]);
      this.scheme = "xyz";
      this.roundZoom = true;
      this.type = "vector";
      this.dispatcher = void 0;
      this.reparseOverscaled = true;
      this._loaded = false;
      this._dataType = "vector";
      this.id = id;
      this._dataType = "vector";
      this.dispatcher = _dispatcher;
      this._implementation = options;

      if (!this._implementation) {
        this.fire(
          new ErrorEvent(
            new Error(`Missing options for ${this.id} ${SOURCE_TYPE} source`),
          ),
        );
      }

      const { url } = options;
      this.reparseOverscaled = true;
      this.scheme = "xyz";
      this.tileSize = 512;
      this._loaded = false;
      this.type = "vector";
      this._protocol = new pmtiles.Protocol();
      this.tiles = [`pmtiles://${url}/{z}/{x}/{y}`];
      const pmtilesInstance = new pmtiles.PMTiles(url);
      this._protocol.add(pmtilesInstance);
      this._instance = pmtilesInstance;
    }

    static async getMetadata(url) {
      const instance = new pmtiles.PMTiles(url);
      return instance.getMetadata();
    }

    static async getHeader(url) {
      const instance = new pmtiles.PMTiles(url);
      return instance.getHeader();
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
      if (!tile.destroy) {
        tile.destroy = () => {};
      }
    }

    async load(callback) {
      this._loaded = false;
      this.fire(new Event("dataloading", { dataType: "source" }));

      return Promise.all([
        this._instance.getHeader(),
        this._instance.getMetadata(),
      ])
        .then(([header, tileJSON]) => {
          extend(this, tileJSON);
          this.header = header;
          const {
            specVersion,
            clustered,
            tileType,
            minZoom,
            maxZoom,
            minLon,
            minLat,
            maxLon,
            maxLat,
            centerZoom,
            centerLon,
            centerLat,
          } = header;
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
            [pmtiles.TileType.Jpeg, pmtiles.TileType.Png].includes(
              this.tileType,
            )
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
            new Event("data", {
              dataType: "source",
              sourceDataType: "metadata",
            }),
          );
          this.fire(
            new Event("data", {
              dataType: "source",
              sourceDataType: "content",
            }),
          );
        })
        .catch((err2) => {
          this.fire(new ErrorEvent(err2));
          if (callback) callback(err2);
        });
    }

    loaded() {
      return this._loaded;
    }

    loadVectorTile(tile, callback) {
      var _a2, _b2, _c;
      const done = (err2, data) => {
        var _a3, _b3;
        delete tile.request;
        if (tile.aborted) return callback(null);
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
        tile.actor = this._tileWorkers[url] =
          this._tileWorkers[url] || this.dispatcher.getActor();
        tile.request = this._protocol.tile({ ...request }, afterLoad);
      } else if (tile.state === "loading") {
        tile.reloadCallback = callback;
      } else {
        tile.request = this._protocol.tile({ ...tile, url }, afterLoad);
      }
    }

    loadRasterTileData(tile, data) {
      tile.setTexture(data, this.map.painter);
    }

    loadRasterTile(tile, callback) {
      var _a2, _b2;
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
        const blob = new window.Blob([new Uint8Array(data)], {
          type: "image/png",
        });
        window
          .createImageBitmap(blob)
          .then((imageBitmap) => {
            this.loadRasterTileData(tile, imageBitmap);
            tile.state = "loaded";
            callback(null);
          })
          .catch((error) => {
            tile.state = "errored";
            return callback(
              new Error(
                `Can't infer data type for ${this.id}, only raster data supported at the moment. ${error}`,
              ),
            );
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
      tile.request = { cancel: () => controller.abort() };
      this._protocol
        .tile(request, controller)
        .then(done.bind(this))
        .catch((error) => {
          if (error.code === 20) return;
          tile.state = "errored";
          callback(error);
        });
    }
  }

  PmTilesSource.SOURCE_TYPE = SOURCE_TYPE;

  // Export to global scope
  global.MapboxPmTilesSource = PmTilesSource;
  global.PMTILES_SOURCE_TYPE = SOURCE_TYPE;
})(typeof window !== "undefined" ? window : this);
