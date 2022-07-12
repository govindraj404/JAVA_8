using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';
using {com.sap.ic.cmh.qualityNotification.dataType as datatype} from '../../../db/cds/qualityNotification/index';

service QualityNotificationService {
    entity QualityNotifications as projection on cmh.qualityNotification.QualityNotifications{
                *,
    @Core.Computed: false 7 as isQualityNotificationFieldControlMandatory : datatype.FieldControl,
    @Core.Computed: false 3 as isQualityNotificationFieldControl : datatype.FieldControl,
    @Core.Computed: false false as isUpdateRestricted : datatype.UpdateRestricted,
    @Core.Computed: false '' as sNavigation         : String(20),
      '' as number : datatype.Identifier
    };
   // entity Defect as projection on cmh.defect.Defects;
    @readonly
    entity QualityStreamStatuses as projection on cmh.qualityStreamStatus.QualityStreamStatuses;
}
