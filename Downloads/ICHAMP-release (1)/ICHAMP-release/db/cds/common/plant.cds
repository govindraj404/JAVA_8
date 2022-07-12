namespace com.sap.ic.cmh.plant;

using {cuid} from '@sap/cds/common';
using {com.sap.ic.cmh.address.Address} from './index';
using {com.sap.ic.cmh.companyCode.CompanyCode} from './index';
using {com.sap.ic.cmh.materialMasterPlantData.MaterialMasterPlantDatas} from './index';
using com.sap.ic.cmh.common.dataType as DataType from './index';
using {com.sap.ic.cmh.storageLocation.StorageLocations} from './index';

type Plant : Association to one Plants;

@cds.search: { 
    plant, plantName
}

entity Plants : cuid {
    plant                   : DataType.Plant;
    plantName               : DataType.PlantName;
    plantNameExtension      : DataType.PlantNameExtension;
    companyCodeID           : CompanyCode;
    customerNoOfPlant       : DataType.CustomerNoOfPlant;
    supplierNoOfPlant       : DataType.SupplierNoOfPlant;
    addressID               : Address;
    factoryCalendar         : DataType.FactoryCalendar;
    materialMasterPlantData : Association to many MaterialMasterPlantDatas on materialMasterPlantData.ID=ID;  
    storageLocation         : Association to many StorageLocations on storageLocation.ID=ID;  
}