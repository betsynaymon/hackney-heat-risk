// ----------------------------------------------
// 06_lst_landsat.js
// Summer 2026 land surface temperature for Hackney's 2021 LSOAs.
// Runs in the Earth Engine Code Editor, not in R.
//
// Input:  Earth Engine asset users/naymonb/hackney_lsoa_2021
//         (data-raw/gee/hackney_lsoa_2021.shp from 05_boundaries.R, uploaded)
// Output: hackney_lst_summer2026.csv, exported to Google Drive,
//         then saved to data-raw/
// ----------------------------------------------

// ---- 1. Which summer has enough clear scenes? ----
var lsoa = ee.FeatureCollection('projects/earthengine-legacy/assets/users/naymonb/hackney_lsoa_2021');
var hackney = lsoa.geometry();

// Landsat 8 and 9, Collection 2 Level 2 (includes surface temperature)
var landsat = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
  .merge(ee.ImageCollection('LANDSAT/LC09/C02/T1_L2'))
  .filterBounds(hackney)
  .filter(ee.Filter.calendarRange(2025, 2026, 'year'))
  .filter(ee.Filter.calendarRange(5, 9, 'month'));

// For each scene: share of Hackney that is clear, and its mean surface temperature
var summarise_scene = function(img) {
  // QA_PIXEL bits 1-4: dilated cloud, cirrus, cloud, cloud shadow
  var clear = img.select('QA_PIXEL').bitwiseAnd(30).eq(0).unmask(0);
  var lst = img.select('ST_B10')
    .multiply(0.00341802).add(149.0).subtract(273.15);   // to °C

  var clear_share = clear.reduceRegion({
    reducer: ee.Reducer.mean(), geometry: hackney, scale: 30
  }).get('QA_PIXEL');

  var mean_lst = lst.updateMask(clear).reduceRegion({
    reducer: ee.Reducer.mean(), geometry: hackney, scale: 30
  }).get('ST_B10');

  return ee.Feature(null, {
    date: img.date().format('YYYY-MM-dd'),
    path: img.get('WRS_PATH'),
    row: img.get('WRS_ROW'),
    clear_share: clear_share,
    mean_lst: mean_lst
  });
};

var scenes = ee.FeatureCollection(landsat.map(summarise_scene))
  .filter(ee.Filter.gte('clear_share', 0.5))
  .sort('date');

print('Scenes at least half clear over Hackney', scenes.size());
print(ui.Chart.feature.byFeature(scenes, 'date',
                                 ['path', 'row', 'clear_share', 'mean_lst'])
  .setChartType('Table'));
  
  
// ---- 2. Summer 2026 composite ----
// June-August 2026 scenes at least 99% clear over Hackney.
// Section 1 found six: 19, 26, 27 June; 12, 29 July; 13 August 2026.
var add_clear_share = function(img) {
  var clear = img.select('QA_PIXEL').bitwiseAnd(30).eq(0).unmask(0);
  var share = clear.reduceRegion({
    reducer: ee.Reducer.mean(), geometry: hackney, scale: 30
  }).get('QA_PIXEL');
  return img.set('clear_share', share);
};

var to_celsius = function(img) {
  var clear = img.select('QA_PIXEL').bitwiseAnd(30).eq(0);
  return img.select('ST_B10')
    .multiply(0.00341802).add(149.0).subtract(273.15)
    .updateMask(clear)
    .rename('lst');
};

var summer = landsat
  .filterDate('2026-06-01', '2026-09-01')
  .map(add_clear_share)
  .filter(ee.Filter.gte('clear_share', 0.99));

print('Scenes in the composite', summer.size());   // expect 6

var lst_summer = summer.map(to_celsius).median().clip(hackney);

// Check: spread of pixel values, to set the colour range
print('Composite 2nd and 98th percentiles (°C)', lst_summer.reduceRegion({
  reducer: ee.Reducer.percentile([2, 98]), geometry: hackney, scale: 30
}));

Map.centerObject(lsoa, 13);
Map.addLayer(lst_summer,
             {min: 35, max: 46, palette: ['ffffb2', 'fecc5c', 'fd8d3c', 'f03b20', 'bd0026']},
             'Surface temperature, summer 2026');
Map.addLayer(lsoa.style({color: '000000', fillColor: '00000000', width: 0.5}),
             {}, 'LSOA outlines');
             
// ---- 3. Average to LSOAs and export ----
var lsoa_lst = lst_summer.reduceRegions({
  collection: lsoa,
  reducer: ee.Reducer.mean().combine(ee.Reducer.count(), '', true),
  scale: 30,
  crs: 'EPSG:27700'
});

// Check: first few rows
print(lsoa_lst.limit(5));

Export.table.toDrive({
  collection: lsoa_lst,
  description: 'hackney_lst_summer2026',
  fileFormat: 'CSV',
  selectors: ['LSOA21CD', 'LSOA21NM', 'mean', 'count']
});