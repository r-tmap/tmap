// Shared screenshot capture module for mapgl widgets
// Used by both maplibregl.js and mapboxgl.js

function waitForMapEvent(map, eventName, timeout = 5000, trigger = null) {
  return new Promise(resolve => {
    let settled = false;

    function finish(status) {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      resolve(status);
    }

    const timer = setTimeout(() => finish('timeout'), timeout);

    map.once(eventName, () => finish(eventName));

    if (typeof trigger === 'function') {
      try {
        trigger();
      } catch (e) {
        finish('trigger-error');
      }
    }
  });
}

function waitForInitialStyleSetup(map, timeout = 10000) {
  return new Promise(resolve => {
    if (!map || map._initialStyleLoaded === true) {
      resolve('ready');
      return;
    }

    let settled = false;

    function finish(status) {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      clearInterval(poller);
      map.off('styledata', checkReady);
      map.off('idle', checkReady);
      map.off('render', checkReady);
      resolve(status);
    }

    function checkReady() {
      if (map._initialStyleLoaded === true) {
        finish('ready');
      }
    }

    const poller = setInterval(checkReady, 50);
    const timer = setTimeout(() => finish('timeout'), timeout);

    map.on('styledata', checkReady);
    map.on('idle', checkReady);
    map.on('render', checkReady);
  });
}

function applyMapScreenshotOptions(map, options) {
  const container = map.getContainer();
  const hiddenElements = [];
  const hiddenBasemapLayers = [];
  const root = document.documentElement;
  const body = document.body;
  const originalBackgrounds = {
    container: container.style.backgroundColor,
    body: body ? body.style.backgroundColor : '',
    root: root ? root.style.backgroundColor : ''
  };

  // Hide controls (nav, fullscreen, screenshot, etc.) but keep legends/attribution based on options
  if (options.hide_controls) {
    container.querySelectorAll('.maplibregl-ctrl-group, .mapboxgl-ctrl-group').forEach(el => {
      // Skip scale bar if include_scale_bar is true
      if (options.include_scale_bar && el.querySelector('.maplibregl-ctrl-scale, .mapboxgl-ctrl-scale')) {
        return;
      }
      hiddenElements.push({ element: el, display: el.style.display });
      el.style.display = 'none';
    });
    // Also hide layers control, measurement box, and geocoder controls
    container.querySelectorAll('.layers-control, .mapgl-measurement-box, .mapgl-coordinates-control, .maplibregl-ctrl-geocoder, .mapboxgl-ctrl-geocoder, .maptiler-ctrl').forEach(el => {
      hiddenElements.push({ element: el, display: el.style.display });
      el.style.display = 'none';
    });
  }

  // Hide legends if requested
  if (!options.include_legend) {
    container.querySelectorAll('.mapboxgl-legend').forEach(el => {
      hiddenElements.push({ element: el, display: el.style.display });
      el.style.display = 'none';
    });
  }

  // Hide basemap layers and set background color if basemap_color is specified
  if (options.basemap_color && map._basemapLayerIds) {
    map._basemapLayerIds.forEach(layerId => {
      try {
        const vis = map.getLayoutProperty(layerId, 'visibility') || 'visible';
        hiddenBasemapLayers.push({ id: layerId, visibility: vis });
        map.setLayoutProperty(layerId, 'visibility', 'none');
      } catch (e) {
        // Layer may have been removed
      }
    });
  }

  // Determine effective background color for html2canvas
  let effectiveBgColor = options.background_color || null;
  if (options.basemap_color) {
    effectiveBgColor = options.basemap_color === 'transparent' ? null : options.basemap_color;
  }

  if (effectiveBgColor !== null) {
    container.style.backgroundColor = effectiveBgColor;
    if (body) body.style.backgroundColor = effectiveBgColor;
    if (root) root.style.backgroundColor = effectiveBgColor;
  } else {
    container.style.backgroundColor = 'transparent';
    if (body) body.style.backgroundColor = 'transparent';
    if (root) root.style.backgroundColor = 'transparent';
  }

  return {
    container,
    effectiveBgColor,
    hiddenElements,
    hiddenBasemapLayers,
    originalBackgrounds
  };
}

function restoreMapScreenshotOptions(map, state) {
  state.hiddenElements.forEach(item => item.element.style.display = item.display);
  state.hiddenBasemapLayers.forEach(item => {
    try {
      map.setLayoutProperty(item.id, 'visibility', item.visibility);
    } catch (e) {}
  });

  state.container.style.backgroundColor = state.originalBackgrounds.container;
  if (document.body) {
    document.body.style.backgroundColor = state.originalBackgrounds.body;
  }
  if (document.documentElement) {
    document.documentElement.style.backgroundColor = state.originalBackgrounds.root;
  }
}

async function prepareMapForScreenshot(map, options) {
  let state;

  try {
    // Wait until the widget's initial style setup is complete.
    // This is important when PMTiles sources are present because source
    // metadata checks are async and controls may be added later.
    await waitForInitialStyleSetup(map, 10000);

    state = applyMapScreenshotOptions(map, options);

    // Attribution is always included to comply with map provider TOS

    // Wait briefly for the map to settle, but don't block indefinitely.
    // Some Linux/headless browser combinations never report a fully idle map
    // for remote basemap styles even though the canvas is renderable.
    if (!map.loaded()) {
      await waitForMapEvent(map, 'idle', 5000);
    }

    // If basemap layers were hidden, wait for re-render
    if (state.hiddenBasemapLayers.length > 0) {
      await waitForMapEvent(map, 'render', 1000, () => map.triggerRepaint());
    }

    // Force render and capture
    await waitForMapEvent(map, 'render', 1000, () => map.triggerRepaint());

    return state;
  } catch (error) {
    if (state) {
      restoreMapScreenshotOptions(map, state);
    }
    throw error;
  }
}

function findClonedCanvas(
  originalRoot,
  clonedDoc,
  clonedElement,
  originalCanvas,
  selector
) {
  if (!originalCanvas) {
    return null;
  }

  if (originalCanvas.id) {
    const clonedById = clonedDoc.getElementById(originalCanvas.id);
    if (clonedById) {
      return clonedById;
    }
  }

  const originalCanvases = Array.from(originalRoot.querySelectorAll(selector));
  const clonedCanvases = Array.from(clonedElement.querySelectorAll(selector));
  const canvasIndex = originalCanvases.indexOf(originalCanvas);

  return canvasIndex >= 0 ? clonedCanvases[canvasIndex] : null;
}

function copyCanvasContents(originalCanvas, clonedCanvas) {
  if (!originalCanvas || !clonedCanvas) {
    return false;
  }

  const ctx = clonedCanvas.getContext('2d');
  if (!ctx) {
    return false;
  }

  ctx.drawImage(originalCanvas, 0, 0);
  return true;
}

function canvasHasVisiblePixels(canvas) {
  if (!canvas) {
    return false;
  }

  const ctx = canvas.getContext('2d');
  if (!ctx) {
    return false;
  }

  try {
    const width = canvas.width;
    const height = canvas.height;
    const sampleCount = 5;

    for (let yStep = 0; yStep < sampleCount; yStep++) {
      for (let xStep = 0; xStep < sampleCount; xStep++) {
        const x = Math.min(
          width - 1,
          Math.floor((xStep / (sampleCount - 1)) * (width - 1))
        );
        const y = Math.min(
          height - 1,
          Math.floor((yStep / (sampleCount - 1)) * (height - 1))
        );
        const pixel = ctx.getImageData(x, y, 1, 1).data;
        if (pixel[3] !== 0) {
          return true;
        }
      }
    }
  } catch (e) {
    return true;
  }

  return false;
}

function getFlowmapBlendMode(deckCanvas) {
  if (!deckCanvas) {
    return null;
  }

  const computedStyle = window.getComputedStyle(deckCanvas);
  const blendMode =
    deckCanvas.style.mixBlendMode ||
    (computedStyle && computedStyle.mixBlendMode) ||
    '';

  if (!blendMode || blendMode === 'normal') {
    return null;
  }

  return blendMode;
}

function compositeFlowmapCanvasForScreenshot(map, state, clonedDoc, clonedElement) {
  const mapCanvasSelector = 'canvas.maplibregl-canvas, canvas.mapboxgl-canvas';
  const originalMapCanvas = state.container.querySelector(mapCanvasSelector);
  const clonedMapCanvas = findClonedCanvas(
    state.container,
    clonedDoc,
    clonedElement,
    originalMapCanvas,
    mapCanvasSelector
  );

  const mapCanvasCopied = copyCanvasContents(originalMapCanvas, clonedMapCanvas);
  const mapCanvasHasContent =
    mapCanvasCopied && canvasHasVisiblePixels(clonedMapCanvas);

  const originalDeckCanvas = map._deckCanvas &&
    state.container.contains(map._deckCanvas) ?
    map._deckCanvas :
    state.container.querySelector('canvas[id^="deck-canvas-"]');

  if (!originalDeckCanvas) {
    return;
  }

  const deckCanvasSelector = 'canvas[id^="deck-canvas-"]';
  const clonedDeckCanvas = findClonedCanvas(
    state.container,
    clonedDoc,
    clonedElement,
    originalDeckCanvas,
    deckCanvasSelector
  );
  const blendMode = getFlowmapBlendMode(originalDeckCanvas);

  copyCanvasContents(originalDeckCanvas, clonedDeckCanvas);

  if (!blendMode || !clonedMapCanvas || !mapCanvasHasContent) {
    copyCanvasContents(originalDeckCanvas, clonedDeckCanvas);
    return;
  }

  const mapCtx = clonedMapCanvas.getContext('2d');
  if (!mapCtx) {
    copyCanvasContents(originalDeckCanvas, clonedDeckCanvas);
    return;
  }

  const previousCompositeOperation = mapCtx.globalCompositeOperation;
  mapCtx.globalCompositeOperation = blendMode;

  if (mapCtx.globalCompositeOperation !== blendMode) {
    mapCtx.globalCompositeOperation = previousCompositeOperation;
    copyCanvasContents(originalDeckCanvas, clonedDeckCanvas);
    return;
  }

  mapCtx.drawImage(
    originalDeckCanvas,
    0,
    0
  );
  mapCtx.globalCompositeOperation = previousCompositeOperation;

  // The flowmap is already blended into the cloned map canvas. Hide the cloned
  // Deck canvas so html2canvas does not draw it a second time in normal mode.
  if (clonedDeckCanvas) {
    clonedDeckCanvas.style.display = 'none';
  }
}

async function captureMapScreenshot(map, options) {
  const state = await prepareMapForScreenshot(map, options);

  try {
    const canvas = await html2canvas(state.container, {
      useCORS: true,
      allowTaint: true,
      backgroundColor: state.effectiveBgColor,
      logging: false,
      scale: options.image_scale || 1,
      onclone: function(clonedDoc, clonedElement) {
        compositeFlowmapCanvasForScreenshot(map, state, clonedDoc, clonedElement);
      }
    });

    restoreMapScreenshotOptions(map, state);
    return canvas;

  } catch (error) {
    restoreMapScreenshotOptions(map, state);
    throw error;
  }
}

async function prepareMapForNativeScreenshot(map, options) {
  window.__mapglScreenshotState = await prepareMapForScreenshot(map, options);
  return true;
}

function restoreMapAfterNativeScreenshot(map) {
  if (window.__mapglScreenshotState) {
    restoreMapScreenshotOptions(map, window.__mapglScreenshotState);
    window.__mapglScreenshotState = null;
  }
}

function downloadScreenshot(canvas, filename) {
  const link = document.createElement('a');
  link.download = `${filename}.png`;
  link.href = canvas.toDataURL('image/png');
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
}

function createScreenshotControl(map, options, isMaplibre = true) {
  const ctrlPrefix = isMaplibre ? 'maplibregl' : 'mapboxgl';

  const btn = document.createElement("button");
  btn.className = `${ctrlPrefix}-ctrl-icon ${ctrlPrefix}-ctrl-screenshot`;
  btn.type = "button";
  btn.title = options.button_title || "Capture screenshot";
  btn.setAttribute("aria-label", options.button_title || "Capture screenshot");
  btn.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M23 19a2 2 0 0 1-2 2H3a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h4l2-3h6l2 3h4a2 2 0 0 1 2 2z"/><circle cx="12" cy="13" r="4"/></svg>`;
  btn.style.cssText = "display:flex;justify-content:center;align-items:center;cursor:pointer;";

  const container = document.createElement("div");
  container.className = `${ctrlPrefix}-ctrl ${ctrlPrefix}-ctrl-group`;
  container.appendChild(btn);

  let capturing = false;
  btn.onclick = async () => {
    if (capturing) return;
    capturing = true;
    btn.style.opacity = "0.5";
    btn.style.cursor = "wait";

    try {
      const canvas = await captureMapScreenshot(map, {
        include_legend: options.include_legend !== false,
        hide_controls: options.hide_controls !== false,
        include_scale_bar: options.include_scale_bar !== false,
        basemap_color: options.basemap_color || null,
        image_scale: options.image_scale || 1
      });
      downloadScreenshot(canvas, options.filename || "map-screenshot");
    } catch (e) {
      console.error("Screenshot capture failed:", e);
    }

    btn.style.opacity = "1";
    btn.style.cursor = "pointer";
    capturing = false;
  };

  const controlObj = {
    onAdd: () => container,
    onRemove: () => {
      if (container.parentNode) {
        container.parentNode.removeChild(container);
      }
    }
  };

  return controlObj;
}
