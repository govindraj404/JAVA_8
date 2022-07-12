namespace com.sap.ic.cmh.costCollector;

using {cuid,managed,Currency} from '@sap/cds/common';

using {com.sap.ic.cmh.complaint.Complaint} from './index';
using {sap.common.CodeList} from '@sap/cds/common';
using com.sap.ic.cmh.complaint.dataType as DataType from './index';
using {com.sap.ic.cmh.unitOfMeasure.UnitOfMeasures} from '../common/index';
using {com.sap.ic.cmh.itemType.ItemTypes} from './index';
using {com.sap.ic.cmh.subItemType.SubItemTypes} from './index';

type CostCollector : Association to one CostCollectors;

entity CostCollectors : cuid, managed {
    itemType_code        : String (10);
    subItemType_code     : String (10);
    subItemType     : Association to one SubItemTypes on subItemType.code = subItemType_code;
    itemType        : Association to one ItemTypes on itemType.code = itemType_code;
    quantity        : DataType.Quantity;
    unit_code       : String(5);
    unit            : Association to one UnitOfMeasures on unit.code = unit_code;
    totalCost       : DataType.TotalCost;
    claim           : UUID;
    currency        : Currency;
    description     : DataType.Description;
    transferToClaim : Boolean;
    parent          : Complaint;
}