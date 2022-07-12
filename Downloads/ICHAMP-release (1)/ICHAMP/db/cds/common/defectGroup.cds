namespace com.sap.ic.cmh.defectGroup;

type DefectGroup : Association to one DefectGroups;

entity DefectGroups {
    key code : String (20);
        description : localized String(40);
}