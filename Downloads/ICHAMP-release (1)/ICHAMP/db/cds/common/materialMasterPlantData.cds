namespace com.sap.ic.cmh.materialMasterPlantData;

using { cuid } from '@sap/cds/common';
using {com.sap.ic.cmh.plant.Plant} from './index';
using com.sap.ic.cmh.common.dataType as DataType from './index';
using {com.sap.ic.cmh.materialMasterGeneralData.MaterialMasterGeneralData} from './index';
using {com.sap.ic.cmh.unitOfMeasure.UnitOfMeasure} from './index';

type MaterialMasterPlantData : Association to one MaterialMasterPlantDatas;

entity MaterialMasterPlantDatas : cuid {
    materialCode            : DataType.MaterialCode;
    materialCodeID          : MaterialMasterGeneralData;
    plantID                 : Plant;
    containerRequirements   : DataType.ContainerRequirements;
    grossWeight             : DataType.GrossWeight;
    weightUnit              : UnitOfMeasure;
    netWeight               : DataType.NetWeight;
    volume                  : DataType.Volume;
    serialNumberProfile     : DataType.SerialNumberProfile;
    reorderPoint            : DataType.ReorderPoint;
    mrpType                 : DataType.MrpType;
    pricingIndicator        : DataType.PricingIndicator;
    standardPrice_price     : DataType.Price;
    movingAveragePrice_price: DataType.Price;
}