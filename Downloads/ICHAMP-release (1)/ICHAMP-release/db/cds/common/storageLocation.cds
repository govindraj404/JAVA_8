namespace com.sap.ic.cmh.storageLocation;

using { cuid } from '@sap/cds/common';
using {com.sap.ic.cmh.plant.Plant} from './index';
using {com.sap.ic.cmh.common.dataType as DataType} from './index';

type StorageLocation : Association to one StorageLocations;

entity StorageLocations : cuid {
    storageLocation     : DataType.storageLocation;
    storageLocationName : DataType.storageLocationName;
    plantCodeID         : Plant;
}