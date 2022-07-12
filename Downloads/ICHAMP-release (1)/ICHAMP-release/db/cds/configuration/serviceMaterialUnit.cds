namespace com.sap.ic.cmh.serviceMaterialUnit;

using {cuid,managed} from '@sap/cds/common';
using com.sap.ic.cmh.configuration.dataType as DataType from './dataType';
using {com.sap.ic.cmh.unitOfMeasure.UnitOfMeasure} from '../common/index';
using {com.sap.ic.cmh.serviceMaterial.ServiceMaterial} from './index';

@cds.search: { identifier, unit, numerator, denominator, serviceMaterial }

type ServiceMaterialUnit : Association to one ServiceMaterialUnits;

entity ServiceMaterialUnits: cuid,managed {
    unit            : UnitOfMeasure;
    numerator       : DataType.Numerator;
    denominator     : DataType.Denominator;
    defaultUnit     : DataType.Flag default false;
    serviceMaterial : ServiceMaterial;
}