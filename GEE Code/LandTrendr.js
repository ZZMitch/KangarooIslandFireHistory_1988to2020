var KangarooIsland = ee.FeatureCollection("users/mitchbon/KangarooIsland");

//////// Kangeroo Island ////////

//////// Initial Setup ////////
// load modules
var ltgee = require('users/emaprlab/public:Modules/LandTrendr.js'); //LandTrendr.js
var batch = require('users/fitoprincipe/geetools:batch'); //batch export tool

// define parameters
var startYear = 1988; //No imagery 1984-1987
var endYear = 2020; //NOTE: 2020 done seperately due to image issue!
var startDay = '12-25';
var endDay = '3-25';

var coords = [[136.477, -36.107], //Bottom left
              [138.185, -36.107], //Bottom right
              [138.185, -35.516], //Top right
              [136.477, -35.516], //Top left
              [136.477, -36.107]]; //Bottom left again
var aoi = ee.Geometry.Polygon(coords);
//var aoi = ee.FeatureCollection('users/mitchbon/KangarooIsland_Boundary').first().geometry();
//var aoi = ee.Geometry.Point(137.095320233614, -35.8526898879667);

var maskThese = ['cloud', 'shadow', 'snow'] //Not masked: water

var bandList = ['NBR']
var index = 'NBR'
var ftvList = ['NBR'] //Can set seperate VIs for create breakpoints and actual values...

var runParams = { 
  maxSegments:            8, //Default = 6
  spikeThreshold:         0.9,
  vertexCountOvershoot:   3,
  preventOneYearRecovery: true, //Default = seems to depend on source...
  recoveryThreshold:      0.75, //Default = 0.25
  pvalThreshold:          0.05,
  bestModelProportion:    0.75,
  minObservationsNeeded:  6
};

// define change parameters
var changeParams = { // May update if needed in publication later...
  delta:  'loss',
  sort:   'greatest',
  year:   {checked:false, start:2000, end:2010},
  mag:    {checked:true,  value:200,  operator: '>', dsnr:false},
  dur:    {checked:true,  value:4,    operator: '<'},
  preval: {checked:false,  value:300,  operator: '>'},
  mmu:    {checked:true,  value:11},
};

// center and zoom the display in case outputs are to be mapped 
Map.centerObject(aoi,10); //Lower = zoomed out
//Map.addLayer(aoi);

// apply LandTrendr.js functions

//////// Build SRcollection ////////
//1988 to 2019 Collection//
var annualSRcollection = ltgee.buildSRcollection(startYear, 2019, startDay, endDay, aoi, maskThese);
//print(annualSRcollection, 'Annual Surface Reflectance Composites'); 

//Special 2019-2020 Collection//
var SRcollection2020 = ltgee.buildSRcollection(endYear, endYear, '1-02', endDay, aoi, maskThese); 
//var SR2020 = SRcollection2020.reduce(ee.Reducer.median()); //Changes data type, band names etc.
//var SR2020 = ee.Image(SRcollection2020.first()); //Best method to extract image
//print(SR2020, '2020 Special Surface Reflectance Composite');

//Merge Collections//
var annualSRcollection2 = annualSRcollection.merge(SRcollection2020);
//var annualSRcollection2 = annualSRcollection + SR2020
print(annualSRcollection2, 'Annual Surface Reflectance Composites (inc. Special 2020)'); 

var sorted = SRcollection2020.sort('system:index'); //Change to annualSRcollection if needed
var scene = sorted.first();

var vizParamsSR = {
  bands: ['B4', 'B2', 'B1'],
  min: 0,
  max: 3000
};

Map.addLayer(scene, vizParamsSR, '421 Yearly SR Composite'); 
//Use this code and above to get a sence of amount/quality of data for each year

//print('Projection of input img, crs, and trans:', scene.projection()); //WGS 84
//print('Scale in meters:', scene.projection().nominalScale()); // 1 degree in m
// Above is the default projection for GEE

//Export 
//batch.Download.ImageCollection.toDrive(annualSRcollection, 'PHD',
//{region: aoi,
//scale: 30}); 
// Then reproject in ArcGIS?

//batch.Download.ImageCollection.toDrive(annualSRcollection, 'PHD', 
//{scale: 30, 
//region: aoi,
//crs: 'EPSG:32753', //UTM Zone 53S - leads to lines in results?
//type: 'float'});
// Current export changes data values... 

//var annualSRcollection_SingleYearTest = ee.Image(annualSRcollection.filterDate('2000-01-01', '2000-12-31'));
//print(annualSRcollection_SingleYearTest);
//Map.addLayer(annualSRcollection_SingleYearTest);

//////// Build ClearPixelCountCollection ////////
var nClearCollection = ltgee.buildClearPixelCountCollection(1988, 2019, startDay, endDay, aoi, maskThese);
//print(nClearCollection, 'Annual Unmasked Pixel Count'); //startYear above

var nClearCollection2020 = ltgee.buildClearPixelCountCollection(endYear, endYear, '1-02', endDay, aoi, maskThese);
//print(nClearCollection2020, '2020 Special Unmasked Pixel Count');

//Merge Collections//
var nClearCollection2 = nClearCollection.merge(nClearCollection2020);
print(nClearCollection2, 'Annual Unmasked Pixel Count (inc. Special 2020)');

var sorted = nClearCollection2.sort('system:index'); //Change to nClearCollection if needed
var scene = sorted.first();

var vizParamsnC = {
  min: 0,
  max: 10
};


Map.addLayer(scene, vizParamsnC, 'Unmasked Pixel Count'); 
//Use this code and above to get a sence of amount/quality of data for each year

Export
Export.image.toDrive({
  image: scene.float(),
  description: 'nClear_2020',
  scale: 30,
  region: KangarooIsland,
}); 

//////// Transform SRcollection ////////
var indexCollection = ltgee.transformSRcollection(annualSRcollection2, bandList);
//var year2000 = ee.Image(indexCollection.filterDate('2000-01-01','2000-12-31').first());
//print(year2000);

//////// Build LTcollection ////////
var annualLTcollection = ltgee.buildLTcollection(annualSRcollection2, index, ftvList);
//print(annualLTcollection, 'Index Surface Reflectance Time-series (+ and -)')
//Map.addLayer(annualLTcollection); //Raw Graphs (both + and -)

//////// collectionToBandStack ////////
var collectionBandStack = ltgee.collectionToBandStack(indexCollection, startYear, endYear);
//print(collectionBandStack, 'Index Surface Reflectance Time-series'); 
//Map.addLayer(collectionBandStack, {}, 'Surface Reflectance Medoid'); //Raw Graphs 
//Map.addLayer(collectionBandStack, {"bands":["2000"],"min":-1000,"max":5000,"palette":["ff2f0d","fff825","0ab308"]});

//Export
//Export.image.toDrive({
//  image: collectionBandStack,
//  description: 'NBR_SR_Medoid_88to20',
//  scale: 30,
//  region: KangarooIsland,
//}); 
//Best option: Export without specifying crs and project in ArcGIS to UTM Zone 53S (billinear)

//////// runLT ////////
var lt = ltgee.runLT(startYear, endYear, startDay, endDay, aoi, index, ftvList, runParams, maskThese);
//print(lt, 'Main LT-GEE Output: LandTrendr Array, Fitted Index Data, RMSE'); //NOTE: id - ftv_tca_fit!! 
//Map.addLayer(lt); //3 Parts: LandTrendr,ftv_nbr_fit, rmse... see LT-GEE Outputs in Guide
//Lt runs by itself (does not reference code above it at all...)
//runParams.timeSeries = annualLTcollection; //IT WORKS!!!
var lt2 = ee.Algorithms.TemporalSegmentation.LandTrendr(runParams);
//print(lt2, 'LandTrendr Result'); //Should be 3 parts
//Map.addLayer(lt2);

//////// getSegmentData ////////
var segInfo = ltgee.getSegmentData(lt, index, 'all');
//print(segInfo, 'Number of Segments in Fitted Time-series'); 
//Map.addLayer(segInfo);

//////// Get FittedData ////////
var nbrFTV = ltgee.getFittedData(lt2, startYear, endYear, ftvList[0]); //lt
print(nbrFTV, 'Index Fitted Time-series'); 
Map.addLayer(nbrFTV, {}, "LandTrendr Fitted"); //Fitted Graphs 

//Export
//Export.image.toDrive({
//  image: nbrFTV,
//  description: 'NBR_Fitted_88to20',
//  scale: 30,
//  region: KangarooIsland,
//}); //Bigger file than medoid data because of decimal places

//////// Mapping ////////
//getChangeMap
//changeParams.index = index;

//var changeImg = ltgee.getChangeMap(lt2, changeParams);

//var palette = ['#9400D3', '#4B0082', '#0000FF', '#00FF00', '#FFFF00', '#FF7F00', '#FF0000'];

//var yodVizParams = {
//  min: startYear,
//  max: endYear,
//  palette: palette
//};

//var magVizParams = {
//  min: 200,
//  max: 1000,
//  palette: palette
//};

//Map.addLayer(changeImg.select(['mag']), magVizParams, 'Magnitude of Change'); //Export
//Map.addLayer(changeImg.select(['yod']), yodVizParams, 'Year of Detection'); //Export

//Export
//Export.image.toDrive({
//  image: changeImg.select(['yod']),
//  description: 'LargestNBRLoss_Year',
//  scale: 30,
//  region: KangarooIsland,
//}); 

// getSegmentCount
//var segCount = ltgee.getSegmentCount(segInfo);
//Map.addLayer(segCount, {"min":0, "max":6});

//Kangaroo Island Boundary
//var empty = ee.Image().byte(); //Create empty image
//var outline = empty.paint({
//  featureCollection: KangarooIsland,
//  color: 1,
//  width: 3
//});

//Map.addLayer(outline, {palette: 'FF0000'}, 'Kangaroo Island');
//Map.addLayer(KangarooIsland, {color: 'FF0000'}, 'Kangaroo Island');