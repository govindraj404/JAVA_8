namespace com.sap.ic.cmh.itemType;

type ItemType : Association to one ItemTypes;

entity ItemTypes {
    key code : String (10);
        description : localized String(60);
}