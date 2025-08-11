// Turf.js operations module for mapgl
// Shared operations that work with both mapboxgl and maplibre maps

// Process turf operations on map initialization (for static maps)
function processTurfOperationsOnLoad(map, turfOperations, widgetId) {
  if (!turfOperations || turfOperations.length === 0) return;
  
  // Wait for map to be fully loaded, then execute operations
  map.on('load', function() {
    // Add a small delay to ensure all layers are loaded
    setTimeout(function() {
      turfOperations.forEach(function(operation) {
        try {
          handleTurfOperation(map, operation, widgetId);
        } catch (error) {
          console.error(`Error processing turf operation ${operation.type}:`, error);
        }
      });
    }, 100);
  });
}

// Main handler for all turf operations
function handleTurfOperation(map, message, widgetId) {
  try {
    switch (message.type) {
      case "turf_buffer":
        executeTurfBuffer(map, message, widgetId);
        break;
      case "turf_union":
        executeTurfUnion(map, message, widgetId);
        break;
      case "turf_intersect":
        executeTurfIntersect(map, message, widgetId);
        break;
      case "turf_difference":
        executeTurfDifference(map, message, widgetId);
        break;
      case "turf_convex_hull":
        executeTurfConvexHull(map, message, widgetId);
        break;
      case "turf_concave_hull":
        executeTurfConcaveHull(map, message, widgetId);
        break;
      case "turf_voronoi":
        executeTurfVoronoi(map, message, widgetId);
        break;
      case "turf_distance":
        executeTurfDistance(map, message, widgetId);
        break;
      case "turf_area":
        executeTurfArea(map, message, widgetId);
        break;
      case "turf_centroid":
        executeTurfCentroid(map, message, widgetId);
        break;
      case "turf_center_of_mass":
        executeTurfCenterOfMass(map, message, widgetId);
        break;
      case "turf_filter":
        executeTurfFilter(map, message, widgetId);
        break;
      default:
        console.warn(`Unknown turf operation: ${message.type}`);
    }
  } catch (error) {
    console.error(`Error executing turf operation ${message.type}:`, error);
    if (HTMLWidgets.shinyMode && message.send_to_r) {
      Shiny.setInputValue(widgetId + "_turf_error", {
        operation: message.type,
        error: error.message,
        timestamp: Date.now()
      });
    }
  }
}

// Helper function to get input data for turf operations
function getInputData(map, message) {
  // If coordinates provided, create point or points client-side
  if (message.coordinates) {
    // Handle single coordinate pair
    if (typeof message.coordinates[0] === 'number') {
      return turf.point(message.coordinates);
    }
    // Handle multiple coordinate pairs
    if (Array.isArray(message.coordinates[0])) {
      const points = message.coordinates.map(coord => turf.point(coord));
      return {
        type: "FeatureCollection",
        features: points
      };
    }
  }
  
  // If GeoJSON data provided directly
  if (message.data) {
    // Check if data is already an object (shouldn't happen) or string
    if (typeof message.data === 'string') {
      return JSON.parse(message.data);
    } else {
      // If it's already an object, return as-is
      return message.data;
    }
  }
  
  // If layer_id provided, get from existing layer
  if (message.layer_id) {
    return getSourceData(map, message.layer_id);
  }
  
  throw new Error("No valid input data provided (coordinates, data, or layer_id)");
}

// Helper function to get source data from a layer
function getSourceData(map, layerId) {
  // First try to get from existing source
  const source = map.getSource(layerId);
  if (source) {
    // Check for _data property (GeoJSON sources)
    if (source._data) {
      return source._data;
    }
    // Check for data property
    if (source.data) {
      return source.data;
    }
  }
  
  // Try with _source suffix (common pattern in mapgl)
  const sourceWithSuffix = map.getSource(layerId + "_source");
  if (sourceWithSuffix) {
    if (sourceWithSuffix._data) {
      return sourceWithSuffix._data;
    }
    if (sourceWithSuffix.data) {
      return sourceWithSuffix.data;
    }
  }
  
  // Query rendered features as fallback
  try {
    const features = map.queryRenderedFeatures({ layers: [layerId] });
    if (features.length > 0) {
      return {
        type: "FeatureCollection",
        features: features
      };
    }
  } catch (e) {
    // Layer might not exist, continue to error
  }
  
  throw new Error(`Could not find source data for layer: ${layerId}`);
}

// Helper function to add result source to map
function addResultSource(map, result, sourceId) {
  if (!sourceId) return;
  
  // Ensure result is valid GeoJSON
  if (!result) {
    result = {
      type: "FeatureCollection",
      features: []
    };
  }
  
  // Check if source exists, update data or create new
  const existingSource = map.getSource(sourceId);
  if (existingSource) {
    // Update existing source data
    existingSource.setData(result);
  } else {
    // Add new source with result data
    map.addSource(sourceId, {
      type: "geojson",
      data: result,
      generateId: true
    });
  }
}

// Helper function to send result to R via Shiny input
function sendResultToR(widgetId, operation, result, metadata = {}, inputId = null) {
  if (HTMLWidgets.shinyMode && inputId) {
    Shiny.setInputValue(widgetId + "_turf_" + inputId, {
      operation: operation,
      result: result,
      metadata: metadata,
      timestamp: Date.now()
    });
  }
}

// Buffer operation
function executeTurfBuffer(map, message, widgetId) {
  const inputData = getInputData(map, message);
  
  const buffered = turf.buffer(inputData, message.radius, {
    units: message.units || "meters"
  });
  
  if (message.source_id) {
    addResultSource(map, buffered, message.source_id);
  }
  
  if (message.input_id) {
    sendResultToR(widgetId, "buffer", buffered, {
      radius: message.radius,
      units: message.units || "meters"
    }, message.input_id);
  }
}

// Union operation
function executeTurfUnion(map, message, widgetId) {
  const inputData = getInputData(map, message);
  
  let result;
  if (inputData.type === "FeatureCollection" && inputData.features.length > 1) {
    // Use turf.union with properly formatted FeatureCollection
    const union = turf.union(turf.featureCollection(inputData.features));
    
    result = union ? {
      type: "FeatureCollection",
      features: [union]
    } : {
      type: "FeatureCollection",
      features: []
    };
  } else if (inputData.type === "FeatureCollection" && inputData.features.length === 1) {
    // Single feature, return as-is
    result = {
      type: "FeatureCollection",
      features: [inputData.features[0]]
    };
  } else {
    // Single feature, return as-is in FeatureCollection
    result = {
      type: "FeatureCollection",
      features: [inputData]
    };
  }
  
  if (message.source_id) {
    addResultSource(map, result, message.source_id);
  }
  
  if (message.input_id) {
    sendResultToR(widgetId, "union", result, {}, message.input_id);
  }
}

// Intersect operation
function executeTurfIntersect(map, message, widgetId) {
  const sourceData1 = getInputData(map, message);
  
  // Get second geometry data
  let sourceData2;
  if (message.data_2) {
    // Handle data_2 directly
    if (typeof message.data_2 === 'string') {
      sourceData2 = JSON.parse(message.data_2);
    } else {
      sourceData2 = message.data_2;
    }
  } else if (message.layer_id_2) {
    // Handle layer_id_2 as before
    sourceData2 = getSourceData(map, message.layer_id_2);
  } else {
    throw new Error("Either data_2 or layer_id_2 must be provided for intersect operation");
  }
  
  // Extract features arrays
  const features1 = sourceData1.type === "FeatureCollection" ? 
    sourceData1.features : [sourceData1];
  const features2 = sourceData2.type === "FeatureCollection" ? 
    sourceData2.features : [sourceData2];
  
  // Collect all intersection results
  const resultFeatures = [];
  
  features1.forEach((feature1, index1) => {
    if (!feature1 || !feature1.geometry) {
      console.warn(`Skipping invalid feature at index ${index1}`);
      return;
    }
    
    features2.forEach((feature2, index2) => {
      if (!feature2 || !feature2.geometry) {
        return;
      }
      
      // Use booleanIntersects for efficient filtering
      if (turf.booleanIntersects(feature1, feature2)) {
        try {
          // Use turf.intersect with options to preserve properties
          const intersection = turf.intersect(
            turf.featureCollection([feature1, feature2]),
            { properties: feature1.properties }
          );
          
          if (intersection) {
            // Ensure properties are preserved (fallback if options didn't work)
            if (!intersection.properties || Object.keys(intersection.properties).length === 0) {
              intersection.properties = { ...feature1.properties };
            }
            resultFeatures.push(intersection);
          }
        } catch (error) {
          console.error(`Error intersecting features ${index1} and ${index2}:`, error);
        }
      }
    });
  });
  
  const result = {
    type: "FeatureCollection",
    features: resultFeatures
  };
  
  if (message.source_id) {
    addResultSource(map, result, message.source_id);
  }
  
  if (message.input_id) {
    sendResultToR(widgetId, "intersect", result, {}, message.input_id);
  }
}

// Difference operation
function executeTurfDifference(map, message, widgetId) {
  const sourceData1 = getInputData(map, message);
  
  // Get second geometry data
  let sourceData2;
  if (message.data_2) {
    // Handle data_2 directly
    if (typeof message.data_2 === 'string') {
      sourceData2 = JSON.parse(message.data_2);
    } else {
      sourceData2 = message.data_2;
    }
  } else if (message.layer_id_2) {
    // Handle layer_id_2 as before
    sourceData2 = getSourceData(map, message.layer_id_2);
  } else {
    throw new Error("Either data_2 or layer_id_2 must be provided for difference operation");
  }
  
  // Extract features arrays
  const features1 = sourceData1.type === "FeatureCollection" ? 
    sourceData1.features : [sourceData1];
  const features2 = sourceData2.type === "FeatureCollection" ? 
    sourceData2.features : [sourceData2];
  
  // Process each feature in features1
  const resultFeatures = [];
  
  features1.forEach((feature1, index) => {
    if (!feature1 || !feature1.geometry) {
      console.warn(`Skipping invalid feature at index ${index}`);
      return;
    }
    
    // Start with the original feature
    let currentFeature = feature1;
    
    // Apply difference with each feature from features2
    for (const feature2 of features2) {
      if (!feature2 || !feature2.geometry || !currentFeature) {
        continue;
      }
      
      // Use booleanIntersects for efficient filtering
      if (turf.booleanIntersects(currentFeature, feature2)) {
        try {
          const diff = turf.difference(turf.featureCollection([currentFeature, feature2]));
          
          if (diff) {
            // Preserve properties from the original feature
            diff.properties = { ...feature1.properties };
            currentFeature = diff;
          } else {
            // Feature was completely erased
            currentFeature = null;
            break;
          }
        } catch (error) {
          console.error("Error in difference operation:", error);
          // Keep the current feature unchanged on error
        }
      }
      // If no intersection, currentFeature remains unchanged
    }
    
    // Add the result if it still exists
    if (currentFeature) {
      resultFeatures.push(currentFeature);
    }
  });
  
  const result = {
    type: "FeatureCollection",
    features: resultFeatures
  };
  
  if (message.source_id) {
    addResultSource(map, result, message.source_id);
  }
  
  if (message.input_id) {
    sendResultToR(widgetId, "difference", result, {}, message.input_id);
  }
}

// Convex hull operation
function executeTurfConvexHull(map, message, widgetId) {
  const inputData = getInputData(map, message);
  
  // Ensure we have valid input data
  if (!inputData) {
    console.warn("No input data for convex hull");
    const result = {
      type: "FeatureCollection",
      features: []
    };
    if (message.source_id) {
      addResultSource(map, result, message.source_id);
    }
    return;
  }
  
  // Check for minimum points if it's a FeatureCollection
  if (inputData.type === "FeatureCollection" && inputData.features.length < 3) {
    console.warn("Convex hull requires at least 3 points, got:", inputData.features.length);
    const result = {
      type: "FeatureCollection",
      features: []
    };
    if (message.source_id) {
      addResultSource(map, result, message.source_id);
    }
    return;
  }
  
  const hull = turf.convex(inputData);
  
  const result = hull ? {
    type: "FeatureCollection",
    features: [hull]
  } : {
    type: "FeatureCollection",
    features: []
  };
  
  if (message.source_id) {
    addResultSource(map, result, message.source_id);
  }
  
  if (message.input_id) {
    sendResultToR(widgetId, "convex_hull", result, {}, message.input_id);
  }
}

// Concave hull operation
function executeTurfConcaveHull(map, message, widgetId) {
  const inputData = getInputData(map, message);
  
  // Ensure we have a FeatureCollection of Points for turf.concave
  let pointCollection;
  if (inputData.type === "FeatureCollection") {
    // Filter to only Point geometries and ensure it's a proper FeatureCollection
    const pointFeatures = inputData.features.filter(feature => 
      feature.geometry && feature.geometry.type === "Point"
    );
    pointCollection = turf.featureCollection(pointFeatures);
  } else if (inputData.type === "Feature" && inputData.geometry.type === "Point") {
    // Single point - wrap in FeatureCollection
    pointCollection = turf.featureCollection([inputData]);
  } else {
    console.warn("Concave hull requires Point geometries, received:", inputData);
    const result = {
      type: "FeatureCollection",
      features: []
    };
    if (message.source_id) {
      addResultSource(map, result, message.source_id);
    }
    return;
  }
  
  // Check if we have enough points (need at least 3 for a hull)
  if (!pointCollection.features || pointCollection.features.length < 3) {
    console.warn("Concave hull requires at least 3 points, got:", pointCollection.features?.length || 0);
    const result = {
      type: "FeatureCollection",
      features: []
    };
    if (message.source_id) {
      addResultSource(map, result, message.source_id);
    }
    return;
  }
  
  // Smart max_edge calculation with fallback
  let hull = null;
  let actualMaxEdge = message.max_edge;
  
  if (message.max_edge) {
    // User specified max_edge, try it first
    hull = turf.concave(pointCollection, {
      maxEdge: message.max_edge,
      units: message.units || "kilometers"
    });
  }
  
  // If no hull or user didn't specify max_edge, try to find optimal value
  if (!hull) {
    // Calculate distances between all points to find reasonable max_edge
    const distances = [];
    const features = pointCollection.features;
    
    for (let i = 0; i < features.length; i++) {
      for (let j = i + 1; j < features.length; j++) {
        const dist = turf.distance(features[i], features[j], {
          units: message.units || "kilometers"
        });
        distances.push(dist);
      }
    }
    
    // Sort distances and try different percentiles as max_edge
    distances.sort((a, b) => a - b);
    const percentiles = [0.6, 0.7, 0.8, 0.9]; // Try 60th, 70th, 80th, 90th percentiles
    
    for (const percentile of percentiles) {
      const index = Math.floor(distances.length * percentile);
      const testMaxEdge = distances[index];
      
      hull = turf.concave(pointCollection, {
        maxEdge: testMaxEdge,
        units: message.units || "kilometers"
      });
      
      if (hull) {
        actualMaxEdge = testMaxEdge;
        console.log(`Auto-calculated max_edge: ${testMaxEdge.toFixed(2)} ${message.units || "kilometers"}`);
        break;
      }
    }
    
    // Final fallback - use convex hull if concave fails
    if (!hull) {
      console.warn("Concave hull failed, falling back to convex hull");
      hull = turf.convex(pointCollection);
      actualMaxEdge = "convex_fallback";
    }
  }
  
  const result = hull ? {
    type: "FeatureCollection",
    features: [hull]
  } : {
    type: "FeatureCollection",
    features: []
  };
  
  if (message.source_id) {
    addResultSource(map, result, message.source_id);
  }
  
  if (message.input_id) {
    sendResultToR(widgetId, "concave_hull", result, {
      max_edge: message.max_edge,
      units: message.units || "kilometers"
    }, message.input_id);
  }
}

// Voronoi operation
function executeTurfVoronoi(map, message, widgetId) {
  const inputData = getInputData(map, message);
  
  const options = {};
  let finalBbox = null;
  let clippingData = null;
  
  // Handle bbox parameter
  if (message.bbox) {
    // Direct bbox array [minX, minY, maxX, maxY]
    options.bbox = message.bbox;
    finalBbox = message.bbox;
  } else if (message.bbox_layer_id) {
    // Extract bbox from layer
    try {
      const bboxSourceData = getSourceData(map, message.bbox_layer_id);
      if (bboxSourceData) {
        // Calculate bbox from layer data
        const bbox = turf.bbox(bboxSourceData);
        options.bbox = bbox;
        finalBbox = bbox;
        // Keep the layer data for potential intersection clipping
        clippingData = bboxSourceData;
      }
    } catch (error) {
      console.warn(`Could not extract bbox from layer ${message.bbox_layer_id}:`, error);
    }
  }
  
  let voronoi = turf.voronoi(inputData, options);
  
  // If we have clipping data (from bbox_layer_id), intersect each Voronoi polygon
  if (voronoi && clippingData && clippingData.type === "FeatureCollection") {
    const clippedFeatures = [];
    
    for (const voronoiFeature of voronoi.features) {
      try {
        // Try to intersect with each feature in the clipping layer
        for (const clipFeature of clippingData.features) {
          const intersection = turf.intersect(turf.featureCollection([voronoiFeature, clipFeature]));
          if (intersection) {
            clippedFeatures.push(intersection);
          }
        }
      } catch (error) {
        // If intersection fails, keep original feature
        clippedFeatures.push(voronoiFeature);
      }
    }
    
    voronoi = {
      type: "FeatureCollection",
      features: clippedFeatures
    };
  }
  
  // If property parameter is provided, use turf.collect to transfer attributes from points to polygons
  if (voronoi && message.property && inputData.type === "FeatureCollection") {
    try {
      // Use turf.collect to gather point properties within each Voronoi polygon
      const collected = turf.collect(voronoi, inputData, message.property, `${message.property}_collected`);
      
      // Since each Voronoi polygon should contain exactly one point, extract the single value from the array
      for (const feature of collected.features) {
        const collectedValues = feature.properties[`${message.property}_collected`];
        if (collectedValues && collectedValues.length > 0) {
          // Take the first (and should be only) value and assign it directly to the property name
          feature.properties[message.property] = collectedValues[0];
          // Remove the temporary array property
          delete feature.properties[`${message.property}_collected`];
        }
      }
      
      voronoi = collected;
    } catch (error) {
      console.warn(`Failed to collect property '${message.property}' from points to Voronoi polygons:`, error);
      // Continue with uncollected voronoi if collection fails
    }
  }
  
  if (message.source_id && voronoi) {
    addResultSource(map, voronoi, message.source_id);
  }
  
  if (message.input_id) {
    sendResultToR(widgetId, "voronoi", voronoi, {
      bbox: finalBbox,
      bbox_layer_id: message.bbox_layer_id
    }, message.input_id);
  }
}

// Distance operation
function executeTurfDistance(map, message, widgetId) {
  let feature1, feature2;
  
  // Get first feature
  if (message.coordinates) {
    feature1 = turf.point(message.coordinates);
  } else if (message.data) {
    const sourceData1 = JSON.parse(message.data);
    feature1 = sourceData1.type === "FeatureCollection" ? 
      sourceData1.features[0] : sourceData1;
  } else if (message.layer_id) {
    const sourceData1 = getSourceData(map, message.layer_id);
    feature1 = sourceData1.type === "FeatureCollection" ? 
      sourceData1.features[0] : sourceData1;
  }
  
  // Get second feature
  if (message.coordinates_2) {
    feature2 = turf.point(message.coordinates_2);
  } else if (message.layer_id_2) {
    const sourceData2 = getSourceData(map, message.layer_id_2);
    feature2 = sourceData2.type === "FeatureCollection" ? 
      sourceData2.features[0] : sourceData2;
  }
  
  const distance = turf.distance(feature1, feature2, {
    units: message.units || "kilometers"
  });
  
  if (message.input_id) {
    sendResultToR(widgetId, "distance", distance, {
      units: message.units || "kilometers"
    }, message.input_id);
  }
}

// Area operation
function executeTurfArea(map, message, widgetId) {
  const inputData = getInputData(map, message);
  
  const area = turf.area(inputData);
  
  if (message.input_id) {
    sendResultToR(widgetId, "area", area, {
      units: "square_meters"
    }, message.input_id);
  }
}

// Centroid operation (using turf.centroid - vertex average method)
function executeTurfCentroid(map, message, widgetId) {
  const inputData = getInputData(map, message);
  
  const centroids = [];
  
  // Handle both single features and FeatureCollections
  const features = inputData.type === "FeatureCollection" ? inputData.features : [inputData];
  
  // Calculate centroid for each individual feature using turf.centroid
  for (const feature of features) {
    const centroid = turf.centroid(feature);
    
    // Preserve all properties from the source feature
    if (feature.properties) {
      centroid.properties = { ...feature.properties };
    }
    
    centroids.push(centroid);
  }
  
  const result = {
    type: "FeatureCollection",
    features: centroids
  };
  
  if (message.source_id) {
    addResultSource(map, result, message.source_id);
  }
  
  if (message.input_id) {
    sendResultToR(widgetId, "centroid", result, {}, message.input_id);
  }
}

// Center of Mass operation (replaces centroid for better accuracy)
function executeTurfCenterOfMass(map, message, widgetId) {
  const inputData = getInputData(map, message);
  
  const centers = [];
  
  // Handle both single features and FeatureCollections
  const features = inputData.type === "FeatureCollection" ? inputData.features : [inputData];
  
  // Calculate center of mass for each individual feature
  for (const feature of features) {
    const centerOfMass = turf.centerOfMass(feature, {});
    
    // Preserve all properties from the source feature
    if (feature.properties) {
      centerOfMass.properties = { ...feature.properties };
    }
    
    centers.push(centerOfMass);
  }
  
  const result = {
    type: "FeatureCollection",
    features: centers
  };
  
  if (message.source_id) {
    addResultSource(map, result, message.source_id);
  }
  
  if (message.input_id) {
    sendResultToR(widgetId, "center_of_mass", result, {}, message.input_id);
  }
}

// Filter operation
function executeTurfFilter(map, message, widgetId) {
  const sourceData = getInputData(map, message);
  
  // Get filter geometry data
  let filterData;
  if (message.filter_data) {
    // Handle filter_data directly
    if (typeof message.filter_data === 'string') {
      filterData = JSON.parse(message.filter_data);
    } else {
      filterData = message.filter_data;
    }
  } else if (message.filter_layer_id) {
    // Handle filter_layer_id as before
    filterData = getSourceData(map, message.filter_layer_id);
  } else {
    throw new Error("Either filter_data or filter_layer_id must be provided for filter operation");
  }
  
  // Extract features arrays
  const features = sourceData.type === "FeatureCollection" ? 
    sourceData.features : [sourceData];
  const filterFeatures = filterData.type === "FeatureCollection" ? 
    filterData.features : [filterData];
  
  // Collect filtered results
  const resultFeatures = [];
  
  features.forEach((feature, index) => {
    if (!feature || !feature.geometry) {
      console.warn(`Skipping invalid feature at index ${index}`);
      return;
    }
    
    // Check if this feature matches the predicate against any filter feature
    let matches = false;
    
    for (const filterFeature of filterFeatures) {
      if (!filterFeature || !filterFeature.geometry) {
        continue;
      }
      
      try {
        // Handle MultiPolygon geometries for within/contains predicates
        if ((feature.geometry.type === 'MultiPolygon' || filterFeature.geometry.type === 'MultiPolygon') &&
            (message.predicate === 'within' || message.predicate === 'contains')) {
          
          // MultiPolygon handling for 'within'
          if (message.predicate === 'within') {
            if (feature.geometry.type === 'MultiPolygon') {
              // All parts of the MultiPolygon must be within the filter
              const polygons = feature.geometry.coordinates.map(coords => ({
                type: 'Feature',
                geometry: { type: 'Polygon', coordinates: coords },
                properties: feature.properties
              }));
              matches = polygons.every(poly => {
                try {
                  return turf.booleanWithin(poly, filterFeature);
                } catch (e) {
                  return false;
                }
              });
            } else if (filterFeature.geometry.type === 'MultiPolygon') {
              // Feature must be within at least one part of the MultiPolygon
              const polygons = filterFeature.geometry.coordinates.map(coords => ({
                type: 'Feature',
                geometry: { type: 'Polygon', coordinates: coords },
                properties: filterFeature.properties
              }));
              matches = polygons.some(poly => {
                try {
                  return turf.booleanWithin(feature, poly);
                } catch (e) {
                  return false;
                }
              });
            }
          }
          // MultiPolygon handling for 'contains'
          else if (message.predicate === 'contains') {
            if (feature.geometry.type === 'MultiPolygon') {
              // At least one part of the MultiPolygon must contain the filter
              const polygons = feature.geometry.coordinates.map(coords => ({
                type: 'Feature',
                geometry: { type: 'Polygon', coordinates: coords },
                properties: feature.properties
              }));
              matches = polygons.some(poly => {
                try {
                  return turf.booleanContains(poly, filterFeature);
                } catch (e) {
                  return false;
                }
              });
            } else if (filterFeature.geometry.type === 'MultiPolygon') {
              // Feature must be contained by at least one part of the MultiPolygon
              const polygons = filterFeature.geometry.coordinates.map(coords => ({
                type: 'Feature',
                geometry: { type: 'Polygon', coordinates: coords },
                properties: filterFeature.properties
              }));
              matches = polygons.some(poly => {
                try {
                  return turf.booleanContains(poly, feature);
                } catch (e) {
                  return false;
                }
              });
            }
          }
        } else {
          // Use the appropriate boolean function based on predicate
          switch (message.predicate) {
            case "intersects":
              matches = turf.booleanIntersects(feature, filterFeature);
              break;
            case "within":
              matches = turf.booleanWithin(feature, filterFeature);
              break;
            case "contains":
              matches = turf.booleanContains(feature, filterFeature);
              break;
            case "crosses":
              matches = turf.booleanCrosses(feature, filterFeature);
              break;
            case "disjoint":
              matches = turf.booleanDisjoint(feature, filterFeature);
              break;
            default:
              console.warn(`Unknown predicate: ${message.predicate}`);
              continue;
          }
        }
        
        if (matches) {
          break; // Found a match, no need to check other filter features
        }
      } catch (error) {
        console.error(`Error testing predicate ${message.predicate}:`, error);
        continue;
      }
    }
    
    // If this feature matches the predicate, add it to results
    if (matches) {
      // Preserve all properties from the original feature
      const resultFeature = {
        ...feature,
        properties: { ...feature.properties }
      };
      resultFeatures.push(resultFeature);
    }
  });
  
  const result = {
    type: "FeatureCollection",
    features: resultFeatures
  };
  
  if (message.source_id) {
    addResultSource(map, result, message.source_id);
  }
  
  if (message.input_id) {
    sendResultToR(widgetId, "filter", result, { 
      predicate: message.predicate,
      filtered_count: resultFeatures.length,
      total_count: features.length 
    }, message.input_id);
  }
}