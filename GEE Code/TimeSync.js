//######################################################################################################## 
//#                                                                                                    #\\
//#                                TIMESYNC LEGACY SOURCE DATA DOWNLOAD                                #\\
//#                                                                                                    #\\
//########################################################################################################


// date: 2018-05-14
// author: Justin Braaten | jstnbraaten@gmail.com
//         Zhiqiang Yang  | zhiqiang.yang@oregonstate.edu
//         Robert Kennedy | rkennedy@coas.oregonstate.edu
// repository: https://code.earthengine.google.com/?accept_repo=users/emaprlab/public: TimeSync-Legacy Data Download Demo
// website: https://github.com/eMapR/TimeSync-Legacy


// about:
//   will export a stack of imagery to be decomposed by an accompanying Python script whose results
//   can be read into TimeSync-Legacy





//########################################################################################################
//############ INPUTS ####################################################################################
//########################################################################################################

var startYear = 1988;
var endYear = 2020;
var startDay = '12-01';
var endDay = '03-01';
var aoi = ee.FeatureCollection('users/mitchbon/KangarooIsland_Boundary').first().geometry();
var description = 'timesync_kangarooisland'; 
var gDriveFolder = 'PHD';
var crs = 'EPSG:32754';
var affine = [30.0, 0, 15.0, 0, -30.0, 15.0];

//########################################################################################################

var ltgee = require('users/emaprlab/public:Modules/LandTrendr.js');
var box = aoi.bounds().buffer(6000).bounds();

Map.centerObject(aoi, 9);
Map.addLayer(aoi);

var tsStack = ltgee.timesyncLegacyStack(startYear, endYear, startDay, endDay, box);

var prefix = startYear.toString()+endYear.toString() + '_' + 
              startDay.replace('-', '') + endDay.replace('-', '') + '_' + 
              crs.replace(':', '') + '_' +
              description;                   

Export.image.toDrive({
  'image': tsStack.clip(box), 
  'region': box, 
  'description': prefix+'_stack', 
  'folder': gDriveFolder, 
  'fileNamePrefix': prefix, 
  'crs': crs, 
  'crsTransform': affine, 
  'maxPixels': 1e13
});


Export.table.toDrive({
  collection: ee.FeatureCollection(box),
  description: prefix+'_bounds',
  folder: gDriveFolder,
  fileNamePrefix: prefix+'_bounds',
  fileFormat: 'GeoJSON'
});