using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';
using {com.sap.ic.cmh.common.dataType as DataType} from '../../../db/cds/common/index';

service MasterDataService{
    entity CompanyCodes as projection on cmh.companyCode.CompanyCodes {
        *,
        @Core.Computed: false '' as address : String
    };
    entity MaterialMasterGeneralDatas as projection on cmh.materialMasterGeneralData.MaterialMasterGeneralDatas;
    entity MaterialMasterPlantDatas as projection on cmh.materialMasterPlantData.MaterialMasterPlantDatas {
        *,
        @Core.Computed: false '' as plant : String,
    };
    entity Plants as projection on cmh.plant.Plants {
        *,
        @Core.Computed: false '' as companyCode : String,
        @Core.Computed: false '' as address : String
    };
    entity BusinessPartners as projection on cmh.businessPartner.BusinessPartners {
        *,
        @Core.Computed: false '' as companyCode : String,
        @Core.Computed: false '' as address : String
    };
    entity PurchaseOrganizations as projection on cmh.purchaseOrganization.PurchaseOrganizations {
        *,
        @Core.Computed: false '' as companyCode : String
    };
    entity Addresses as projection on cmh.address.Addresses;
    entity StorageLocations as projection on cmh.storageLocation.StorageLocations {
        *,
        @Core.Computed: false '' as plantCode : String
    };
    entity PurchasingGroups as projection on cmh.purchasingGroup.PurchasingGroups;
    entity Reasons as projection on cmh.reason.Reasons;
    entity DefectCodes as projection on cmh.defectCode.DefectCodes;
    entity DefectGroups as projection on cmh.defectGroup.DefectGroups;
    entity SubItemTypes as projection on cmh.subItemType.SubItemTypes;
    entity ActionPreconditions as projection on cmh.actionPrecondition.ActionPreconditions;
    entity Actions as projection on cmh.action.Actions;
    entity SalesOrganizations as projection on cmh.salesOrganization.SalesOrganizations{
       *,
       @Core.Computed: false '' as companyCode : DataType.CompanyCode,
       @Core.Computed: false '' as businessPartner : DataType.BusinessPartnerNumber
    };
    entity DistributionChannels as projection on cmh.distributionChannel.DistributionChannels {
        *,
        @Core.Computed: false '' as salesOrganization : DataType.SalesOrganization
    };
    entity Divisions as projection on cmh.division.Divisions{
         *,
        @Core.Computed: false '' as salesOrganization : DataType.SalesOrganization
    };
}