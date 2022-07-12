namespace com.sap.ic.cmh.complaint;

using {
    cuid,
    managed,
    Currency
} from '@sap/cds/common';

using {com.sap.ic.cmh.businessObject.BusinessObjects} from './index';
using {com.sap.ic.cmh.businessPartner.BusinessPartner} from '../common/index';
using {com.sap.ic.cmh.companyCode.CompanyCode} from '../common/index';
using {com.sap.ic.cmh.stream.Streams} from './index';
using {com.sap.ic.cmh.costCollector.CostCollectors} from './index';
using {com.sap.ic.cmh.complaint.dataType as DataType} from './index';
using {com.sap.ic.cmh.plant.Plant} from '../common/index';
using {com.sap.ic.cmh.purchaseOrganization.PurchaseOrganization} from '../common/index';
using {com.sap.ic.cmh.complaintCategory.ComplaintCategory} from '../common/index';
using {com.sap.ic.cmh.complaintStatus.ComplaintStatus} from './index';
using {com.sap.ic.cmh.materialMasterGeneralData.MaterialMasterGeneralData} from '../common/index';
using {com.sap.ic.cmh.unitOfMeasure.UnitOfMeasure} from '../common/index';

type Complaint : Association to one Complaints;

@cds.search    : {
    identifier,
    complaintType,
    description,
    complaintStatus,
    referenceNumber,
    supplier,
    material,
    quantity,
    unit,
    plant,
    purchasingOrganization,
    companyCode,
    personResponsible_ID,
    note,
    creationType,
    contactPerson,
    totalSubLetCost,
    totalLaborHour,
    currency,
    businessObjects.businessObjectID.identifier,
    businessObjects.businessObjectID.status_code,
    businessObjects.businessObjectID.status.name
}

@assert.unique : {identifier : [identifier]}

entity Complaints : cuid, managed {
    identifier             : DataType.Identifier;
    complaintType          : ComplaintCategory;
    description            : DataType.Description;
    complaintStatus        : ComplaintStatus;
    referenceNumber        : DataType.Number;
    supplier               : BusinessPartner;
    material               : MaterialMasterGeneralData;
    quantity               : DataType.Quantity;
    unit                   : UnitOfMeasure;
    plant                  : Plant;
    purchasingOrganization : PurchaseOrganization;
    companyCode            : CompanyCode;
    personResponsible_ID   : DataType.Email; //TODO after Master Data of responsible person is finailised
    note                   : DataType.Note;
    creationType           : DataType.CreationType;
    contactPerson          : BusinessPartner;
    totalSubLetCost        : DataType.totalSubLetCost;
    totalLaborHour         : DataType.TotalLaborHour;
    currency               : Currency;
    laborUnit              : UnitOfMeasure;
    streams                : Composition of many Streams
                                 on streams.parentID = $self;
    costCollector          : Composition of many CostCollectors
                                 on costCollector.parent = $self;
    businessObjects        : Association to many BusinessObjects
                                 on businessObjects.complaint = ID;
}
