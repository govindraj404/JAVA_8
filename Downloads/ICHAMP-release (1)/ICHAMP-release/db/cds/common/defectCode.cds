namespace com.sap.ic.cmh.defectCode;

using {com.sap.ic.cmh.defectGroup.DefectGroups} from './index';

type DefectCode : Association to one DefectCodes;

entity DefectCodes {
    key code             : String(10);
    key defectGroup_code : String(20);
        description      : localized String(40);
        defectGroup      : Association to one DefectGroups
                               on defectGroup.code = defectGroup_code;
}
