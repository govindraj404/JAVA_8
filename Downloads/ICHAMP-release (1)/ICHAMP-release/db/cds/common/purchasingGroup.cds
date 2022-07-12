namespace com.sap.ic.cmh.purchasingGroup;

type PurchasingGroup : Association to one PurchasingGroups;

@cds.search: { 
    code
}
entity PurchasingGroups {
    key code : String (10);
        description : localized String(40);
}