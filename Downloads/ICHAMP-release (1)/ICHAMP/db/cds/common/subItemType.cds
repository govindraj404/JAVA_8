namespace com.sap.ic.cmh.subItemType;

using {com.sap.ic.cmh.itemType.ItemType} from './index';

type SubItemType : Association to one SubItemTypes;

entity SubItemTypes {
    key code : String (10);
    description : localized String(40);
    itemType    : ItemType;
}