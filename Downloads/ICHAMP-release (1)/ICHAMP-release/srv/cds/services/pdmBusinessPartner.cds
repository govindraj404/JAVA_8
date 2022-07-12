using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';

service PdmBusinessPartnerService {

    entity BusinessPartners as projection on cmh.businessPartner.BusinessPartners{
            key businessPartnerNumber as businessPartnerNumber,
            businessPartnerName1 as businessPartnerName1,
            businessPartnerName2 as businessPartnerName2,
            businessPartnerType as businessPartnerType,
            addressID.email as email,
            addressID.mobile as mobile



   };
    entity PDMComplaint   as
    select from cmh.complaint.Complaints{
        key ID,
        identifier as identifier,
        supplier.businessPartnerNumber as supplierNumber,
        supplier.businessPartnerName1 as supplierName,
        supplier.businessPartnerType as supplierType,
        contactPerson.businessPartnerNumber as contactPersonNumber,
        contactPerson.businessPartnerName1 as contactPersonName,
        contactPerson.businessPartnerType as contactPersontype,
        personResponsible_ID as personResponsible,
        createdBy,
        createdAt,
        modifiedAt,
        modifiedBy
    };

    entity PDMQualityNotification as
    select from cmh.qualityNotification.QualityNotifications{
         key ID ,
         identifier as identifier,
         personResponsible.businessPartnerName1 as partnerName,
         personResponsible.businessPartnerNumber as partnerNumber,
         personResponsible.businessPartnerType as partnerType,
         supplier.businessPartnerNumber as supplierNumber,
         supplier.businessPartnerName1 as supplierName,
         supplier.businessPartnerType as supplierType,
         contactPerson.businessPartnerNumber as contactPersonNumber,
         contactPerson.businessPartnerName1 as contactPersonName,
         contactPerson.businessPartnerType as contactPersontype,
         createdAt,
         createdBy,
         modifiedAt,
         modifiedBy
    };

    entity PDMReturnPurchaseOrder as
    select from cmh.returnPurchaseOrder.ReturnPurchaseOrders{
        key ID  ,
        identifier as identifier,
        personResponsible.businessPartnerName1 as partnerName,
        personResponsible.businessPartnerNumber as partnerNumber,
        personResponsible.businessPartnerType as partnerType,
        supplier.businessPartnerNumber as supplierNumber,
        supplier.businessPartnerName1 as supplierName,
        supplier.businessPartnerType as supplierType,
        contactPerson.businessPartnerNumber as contactPersonNumber,
        contactPerson.businessPartnerName1 as contactPersonName,
        contactPerson.businessPartnerType as contactPersontype,
        createdAt,
        createdBy,
        modifiedAt,
        modifiedBy
    };

    entity PDMSupplierIssueProcess  as
    select from cmh.supplierIssueProcess.SupplierIssueProcesses{
        key ID,
        identifier as identifier,
        personResponsible.businessPartnerName1 as partnerName,
        personResponsible.businessPartnerNumber as partnerNumber,
        personResponsible.businessPartnerType as partnerType,
        supplier.businessPartnerNumber as supplierNumber,
        supplier.businessPartnerName1 as supplierName,
        supplier.businessPartnerType as supplierType,
        contactPerson.businessPartnerNumber as contactPersonNumber,
        contactPerson.businessPartnerName1 as contactPersonName,
        contactPerson.businessPartnerType as contactPersontype,
        createdAt,
        createdBy,
        modifiedAt,
        modifiedBy

    };

    entity PDMClaim  as
    select from cmh.claim.Claims{
        key ID,
        identifier as identifier,
        personResponsible.businessPartnerName1 as partnerName,
        personResponsible.businessPartnerNumber as partnerNumber,
        personResponsible.businessPartnerType as partnerType,
        supplier.businessPartnerNumber as supplierNumber,
        supplier.businessPartnerName1 as supplierName,
        supplier.businessPartnerType as supplierType,
        contactPerson.businessPartnerNumber as contactPersonNumber,
        contactPerson.businessPartnerName1 as contactPersonName,
        contactPerson.businessPartnerType as contactPersontype,
        createdAt,
        createdBy,
        modifiedAt,
        modifiedBy

    };


}