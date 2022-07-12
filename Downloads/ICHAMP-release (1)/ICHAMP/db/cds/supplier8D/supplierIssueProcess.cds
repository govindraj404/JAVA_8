namespace com.sap.ic.cmh.supplierIssueProcess;

using {
    cuid,
    managed
} from '@sap/cds/common';

using {com.sap.ic.cmh.businessObjectStatus.BusinessObjectStatuses} from '../common/businessObjectStatus';
using com.sap.ic.cmh.businessPartner.BusinessPartner from '../common/index';
using {com.sap.ic.cmh.plant.Plant} from '../common/index';
using {com.sap.ic.cmh.companyCode.CompanyCode} from '../common/index';
using {com.sap.ic.cmh.purchaseOrganization.PurchaseOrganization} from '../common/index';
using {com.sap.ic.cmh.supplier8D.dataType as DataType} from './index';
using {com.sap.ic.cmh.materialMasterGeneralData.MaterialMasterGeneralData} from '../common/index';
using {com.sap.ic.cmh.complaint.Complaint} from '../complaint/index';
using {com.sap.ic.cmh.defect.Defects} from '../qualityNotification/index';
using {com.sap.ic.cmh.businessObjectRelation.BusinessObjectRelations} from '../common/index';
using {com.sap.ic.cmh.supplierIssueProcessStatus.SupplierIssueProcessStatus} from './supplierIssueProcessStatus';

type SupplierIssueProcess : Association to one SupplierIssueProcesses;

entity SupplierIssueProcesses : cuid, managed {
    identifier                     : DataType.Identifier;
    supplierIssueProcessesType     : DataType.Type;
    supplier                       : BusinessPartner;
    status                         : SupplierIssueProcessStatus;
    quantity                       : DataType.Quantity;
    company                        : CompanyCode;
    unit                           : DataType.Unit;
    material                       : MaterialMasterGeneralData;
    plant                          : Plant;
    requestStartDate               : Date;
    requestEndDate                 : Date;
    actualStartDate                : Date;
    actualEndDate                  : Date;
    personResponsible              : BusinessPartner; //TODO after Master Data of responsible person is finailised
    contactPerson                  : BusinessPartner;
    purchasingOrganization         : PurchaseOrganization;
    complaint                      : Complaint;
    defect                         : Association to one Defects;
    businessObjectStatuses         : Composition of many BusinessObjectStatuses
                                         on businessObjectStatuses.parent = ID;
    businessObjectRelations        : Association to many BusinessObjectRelations
                                         on businessObjectRelations.sourceBusinessObjectUUID = ID;
}
