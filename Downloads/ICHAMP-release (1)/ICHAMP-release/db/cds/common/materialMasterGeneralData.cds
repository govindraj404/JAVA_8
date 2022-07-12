namespace com.sap.ic.cmh.materialMasterGeneralData;

using {cuid} from '@sap/cds/common';
using com.sap.ic.cmh.common.dataType as DataType from './dataType';
using {com.sap.ic.cmh.unitOfMeasure.UnitOfMeasure} from './index';

type MaterialMasterGeneralData : Association to one MaterialMasterGeneralDatas;

@cds.search: { 
    materialCode, materialType, materialGroup
}
entity MaterialMasterGeneralDatas : cuid {
    materialCode                : localized DataType.MaterialCode;
    materialDescription         : localized DataType.MaterialDescription;
    materialType                : DataType.MaterialType;
    materialGroup               : DataType.MaterialGroup;
    baseUnitOfMeasure           : UnitOfMeasure;
    division                    : DataType.Division;
    xPlantMaterialStatus        : DataType.XPlantMaterialStatus;
    validFrom                   : DataType.ValidFrom;
    grossWeight                 : DataType.GrossWeight;
    weightUnit                  : UnitOfMeasure;
    netWeight                   : DataType.NetWeight;
    volume                      : DataType.Volume;
    volumeUnit                  : UnitOfMeasure;
    sizeDimensions              : DataType.SizeDimensions;
    EAN_UPC                     : DataType.EAN_UPC;
    EANCategary                 : DataType.EANCategary;
    productCompositionIndicator : DataType.ProductCompositionIndicator;
    packagingMaterialGroup      : DataType.PackagingMaterialGroup;
}
