namespace com.sap.ic.cmh.serviceMaterialDestination;

using {com.sap.ic.cmh.serviceMaterial.ServiceMaterials} from '../configuration/index';
using {com.sap.ic.cmh.serviceMaterialUnit.ServiceMaterialUnits} from '../configuration/index';
using {com.sap.ic.cmh.destinationConfiguration.DestinationConfigurations} from '../configuration/index';

type ServiceMaterialDestination : Association to one ServiceMaterialDestinations;

@cds.autoexpose
view ServiceMaterialDestinations as select from ServiceMaterialUnits
    left outer join ServiceMaterials
    on ServiceMaterialUnits.serviceMaterial.ID = ServiceMaterials.ID
    left outer join DestinationConfigurations
    on DestinationConfigurations.destination = ServiceMaterials.destination
    and DestinationConfigurations.businessObjectType.code = 'CLM'

    {
        key ServiceMaterialUnits.ID as ID,
        ServiceMaterials.ID as serviceMaterialID,
       DestinationConfigurations.ID as destinationConfigurationID,
       DestinationConfigurations.destination as destination,
       DestinationConfigurations.companyCode as companyCode,
       ServiceMaterials.itemType as itemType,
       ServiceMaterials.subItemType as subItemType,
       ServiceMaterialUnits.unit as unit,
       ServiceMaterialUnits.defaultUnit as defaultUnit
    };