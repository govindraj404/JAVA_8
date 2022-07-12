namespace com.sap.ic.cmh.defect;

using {
    cuid,
    managed
} from '@sap/cds/common';

using {com.sap.ic.cmh.defectGroup.DefectGroups} from '../common/index';
using {com.sap.ic.cmh.defectCode.DefectCodes} from '../common/index';
using {com.sap.ic.cmh.qualityNotification.QualityNotifications} from './index';
using {com.sap.ic.cmh.qualityNotification.dataType as DataType} from './index';
using {com.sap.ic.cmh.businessObjectStatus.BusinessObjectStatuses} from '../common/index';

type Defect : Association to one Defects;

entity Defects : cuid, managed {
    identifier             : DataType.Identifier;
    defectGroup_code       : String(20);
    defectCode_code        : String(10);
    defectCode             : Association to one DefectCodes
                                 on  defectCode.code             = defectCode_code
                                 and defectCode.defectGroup_code = defectGroup_code;
    defectGroup            : Association to one DefectGroups
                                 on defectGroup.code = defectGroup_code;
    description            : DataType.Description;
    personResponsible      : UUID;
    parent                 : Association to QualityNotifications;
    businessObjectStatuses : Composition of many BusinessObjectStatuses
                                 on businessObjectStatuses.parent = ID;
}
