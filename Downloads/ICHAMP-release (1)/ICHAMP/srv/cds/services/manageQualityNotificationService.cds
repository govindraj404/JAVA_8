using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';
using {com.sap.ic.cmh.common.dataType as DataType} from '../../../db/cds/common/index';
using {com.sap.ic.cmh.complaint.dataType as Dt} from '../../../db/cds/complaint/index';

service ManageQualityNotificationService {
    @Capabilities.Deletable : false
    entity QualityNotifications as projection on cmh.qualityNotification.QualityNotifications{
    *,
    @Core.Computed: false '' as contactPersonCode : DataType.BusinessPartnerNumber,
    @Core.Computed: false '' as complaintCode : Dt.Identifier,
    @Core.Computed: false '' as personResponsibleCode : DataType.BusinessPartnerNumber,
    @Core.Computed: false '' as supplierCode : DataType.BusinessPartnerNumber,
    @Core.Computed: false '' as materialCode : DataType.MaterialCode,
    @Core.Computed: false '' as plantCode : DataType.Plant,
    @Core.Computed: false '' as purchaseOrganisationCode : DataType.PurchagingOrganization
    };
}