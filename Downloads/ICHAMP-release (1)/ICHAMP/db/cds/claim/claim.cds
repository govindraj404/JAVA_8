namespace com.sap.ic.cmh.claim;

using {
  cuid,
  managed
} from '@sap/cds/common';

using {com.sap.ic.cmh.businessObjectStatus.BusinessObjectStatuses} from '../common/index';
using {com.sap.ic.cmh.companyCode.CompanyCode} from '../common/index';
using {com.sap.ic.cmh.businessPartner.BusinessPartner} from '../common/index';
using {com.sap.ic.cmh.costCollector.CostCollectors} from '../complaint';
using {com.sap.ic.cmh.claim.dataType as DataType} from './dataType';
using {com.sap.ic.cmh.itemType.ItemType} from '../common/index';
using {com.sap.ic.cmh.plant.Plant} from '../common/index';
using {com.sap.ic.cmh.claimStatus.ClaimStatus} from './index';
using {com.sap.ic.cmh.purchaseOrganization.PurchaseOrganization} from '../common/index';
using {com.sap.ic.cmh.materialMasterGeneralData.MaterialMasterGeneralData} from '../common/index';
using {com.sap.ic.cmh.complaint.Complaint} from '../complaint/index';
using {com.sap.ic.cmh.businessObjectRelation.BusinessObjectRelations} from '../common/index';

type Claim : Association to one Claims;

entity Claims : cuid, managed {
  identifier                  : DataType.Identifier;
  claimType                   : DataType.Type;
  supplierRole                : DataType.SupplierRole;
  versionCategory             : DataType.VersionCategory;
  supplier                    : BusinessPartner;
  material                    : MaterialMasterGeneralData;
  quantity                    : DataType.Quantity;
  unit                        : DataType.Unit;
  plant                       : Plant;
  purchasingOrganization      : PurchaseOrganization;
  itemType                    : ItemType;
  personResponsible           : BusinessPartner; //TODO after Master Data of responsible person is finailised
  requestedAmount             : DataType.Amount;
  paidAmount                  : DataType.Amount;
  decision                    : DataType.Decision;
  contactPerson               : BusinessPartner;
  requestSentDate             : Date;
  responseReceivedDate        : Date;
  status                      : ClaimStatus;
  company                     : CompanyCode;
  complaint                   : Complaint;
  costCollectors              : Association to many CostCollectors on costCollectors.parent.ID = complaint.ID;
  businessObjectStatuses      : Composition of many BusinessObjectStatuses on businessObjectStatuses.parent = ID;
  businessObjectRelations : Association to many BusinessObjectRelations on businessObjectRelations.sourceBusinessObjectUUID = ID;
}
