namespace com.sap.ic.cmh.destinationConfiguration;

using {
    cuid,
    managed
} from '@sap/cds/common';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';
using {com.sap.ic.cmh.businessObjectType.BusinessObjectType} from '../complaint/index';
using {com.sap.ic.cmh.companyCode.CompanyCode} from '../common/index';

@cds.search : {
    identifier,
    companyCode,
    businessObjectType,
    destination,
    description
}

entity DestinationConfigurations : cuid, managed {
    identifier            : DataType.Identifier;
    companyCode           : CompanyCode;
    businessObjectType    : BusinessObjectType;
    destination           : DataType.Destination;
    description           : DataType.Description;
    navigationDestination : DataType.Destination;
}
