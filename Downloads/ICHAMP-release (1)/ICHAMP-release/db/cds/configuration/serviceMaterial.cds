namespace com.sap.ic.cmh.serviceMaterial;

using {cuid,managed} from '@sap/cds/common';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';
using {com.sap.ic.cmh.itemType.ItemType} from '../common/index';
using {com.sap.ic.cmh.subItemType.SubItemType} from '../common/index';
using {com.sap.ic.cmh.serviceMaterialUnit.ServiceMaterialUnits} from './index';

@cds.search: { identifier, destination, subItemType, itemType, serviceMaterial,description }

type ServiceMaterial : Association to one ServiceMaterials;

entity ServiceMaterials : cuid, managed {
    identifier      : DataType.Identifier;
    destination     : DataType.Destination;
    subItemType     : SubItemType;
    itemType        : ItemType;
    serviceMaterial : DataType.ServiceMaterial;
    description     : DataType.Description;
    serviceMaterialUnit : Composition of many ServiceMaterialUnits on serviceMaterialUnit.serviceMaterial = $self;
}