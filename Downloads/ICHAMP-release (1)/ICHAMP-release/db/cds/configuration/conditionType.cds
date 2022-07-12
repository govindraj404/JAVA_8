namespace com.sap.ic.cmh.conditionType;

using { cuid, managed } from '@sap/cds/common';
using com.sap.ic.cmh.configuration.dataType as DataType from './index';
using {com.sap.ic.cmh.itemType.ItemType} from '../common/index';

@cds.search: { identifier, destination, itemType, conditionType, description }

entity ConditionTypes : cuid, managed {
    identifier    : DataType.Identifier;
    destination   : DataType.Destination;
    itemType      : ItemType;
    conditionType : DataType.ConditionType;
    description   : DataType.Description;
}