namespace com.sap.ic.cmh.subItemTypeUnit;

using {com.sap.ic.cmh.serviceMaterialUnit.ServiceMaterialUnits} from '../configuration/index';
using {com.sap.ic.cmh.serviceMaterial.ServiceMaterials} from '../configuration/index';
using {com.sap.ic.cmh.subItemType.SubItemTypes} from '../configuration/index';
using {com.sap.ic.cmh.serviceMaterialDestination.ServiceMaterialDestinations} from './index';
using {com.sap.ic.cmh.destinationConfiguration.DestinationConfigurations} from '../configuration/index';

view SubItemTypesUnits as select from SubItemTypes
   left outer join ServiceMaterials
   on ServiceMaterials.subItemType.code = SubItemTypes.code
   and ServiceMaterials.itemType.code = SubItemTypes.itemType.code
   left outer join DestinationConfigurations
    on DestinationConfigurations.destination = ServiceMaterials.destination
    and DestinationConfigurations.businessObjectType.code = 'CLM'
    left outer join ServiceMaterialDestinations
   on ServiceMaterialDestinations.serviceMaterialID = ServiceMaterials.ID
   and ServiceMaterialDestinations.defaultUnit = true
   {
      key SubItemTypes.code as code,
      key case 
      when ServiceMaterials.ID is null 
      then SubItemTypes.code 
      else
        ServiceMaterials.ID
        end as serviceMaterialID: String(40),
      SubItemTypes.itemType as itemType,
      SubItemTypes.description as description,
      ServiceMaterialDestinations.unit as unit,
      DestinationConfigurations.companyCode as companyCode
   };