package com.sap.ic.cmh.managequalitynotification.handler;

import cds.gen.managequalitynotificationservice.QualityNotifications;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.qualitynotificationservice.Defects;
import com.sap.cds.Result;  
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.ic.cmh.qualitynotification.handler.QualityNotificationHandler;
import com.sap.ic.cmh.managequalitynotification.service.ManageQualityNotificationService;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectServiceImpl;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import static org.mockito.ArgumentMatchers.any;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import org.springframework.beans.factory.annotation.Autowired;
import com.sap.ic.cmh.configuration.service.MessageService;
import com.sap.ic.cmh.configuration.model.MasterData;
import cds.gen.managecomplaintservice.Complaints;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class ManageQualityNotificationHandlerTest {

    @InjectMocks
    @Autowired
    ManageQualityNotificationHandler handler;

    @Mock
    QualityNotificationHandler qualityNotifcationHandler;

    @Mock
    ManageQualityNotificationService manageQualityNotificationService;

    @Mock
    CdsCreateEventContext createContextMock;

    @Mock
    CommonFunctions commonFunctions;

    @Mock
    ConfigurationService configurationService;

    @Mock
    ComplaintsDao complaintDao;

    @Mock
    MessageService messageService;

    @Mock
    BusinessObjectServiceImpl businessObjectService;

    @Mock
    protected PersistenceService mockDb;
    @Mock
	QualityNotificationService qualityNotificationService;


    @Mock
    Result result;

    @Mock
    AuditLogHelper auditLogHelper;

    @Mock 
    private AuditLogDifference auditLogDifference;

    @Mock 
    private AuditLogDifference auditLogDifferenceDefects;

    private QualityNotifications qn;
    private Complaints complaints;
    private Defects defects;
    private MasterData masterData;
    cds.gen.qualitynotificationservice.QualityNotifications qualityNotifications;

    private Row row;
    private Optional<Row> opt;
    private BusinessPartners partner;

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        qn = Struct.create(QualityNotifications.class);
        qn.setId("1234");
        qn.setCompanyId("F003");
        qn.setComplaintCode("D345");
        //qn.setContactPerson("person name");


        defects = Struct.create(Defects.class);
        defects.setId("F099");

        qualityNotifications = Struct.create(cds.gen.qualitynotificationservice.QualityNotifications.class);
        qualityNotifications.setCompanyId("F001");
        qualityNotifications.setComplaintId("F9765");

        partner = Struct.create(BusinessPartners.class);
        partner.setId("1234");

        MaterialMasterGeneralDatas material = Struct.create(MaterialMasterGeneralDatas.class);
        material.setMaterialCode("2344");

        PurchaseOrganizations orgs = Struct.create(PurchaseOrganizations.class);
        orgs.setPurchaseOrganization("FGR");

        Plants plant = Struct.create(Plants.class);
        plant.setPlant("plat");
        plant.setId("67");
        masterData = new MasterData();
        masterData.setPlants(plant);
        masterData.setMaterial(material);
        masterData.setPurchaseOrg(orgs);
        masterData.setSupplier(partner);

        row = Struct.create(Row.class);

        complaints = Struct.create(Complaints.class);
    }

    @Test
    public void testBeforeManageQualityNotificationCreateException() {
        qn.setComplaintCode("");
        when(commonFunctions
                .convertManageDefectsToDraftDefects(qn.getDefect())).thenReturn(defects);
         when(manageQualityNotificationService.getMasterDataIdsBasedOnDetails(qn)).thenReturn(masterData);
        when(commonFunctions
                .convertManageQualitynotificationtoQualityNotifications(qn)).thenReturn(qualityNotifications);
        handler.beforeManageQualityNotificationCreate(qn,createContextMock);
    }

    @Test
    public void testBeforeManageQualityNotificationCreate() {
        qn.setComplaintId("F9765");

        when(commonFunctions
                .convertManageDefectsToDraftDefects(qn.getDefect())).thenReturn(defects);

        when(commonFunctions
                .convertManageQualitynotificationtoQualityNotifications(qn)).thenReturn(qualityNotifications);
        when(complaintDao.getComplaintBasedOnCode(qn.getComplaintCode())).thenReturn(result);
        handler.beforeManageQualityNotificationCreate(qn,createContextMock);
    }


    @Test
    public void testBeforeManageQualityNotificationCreateCompPresent() {
        qn.setComplaintId("F9765");
        qn.setContactPersonCode("C234567");
        qn.setPersonResponsibleCode("C345");

        when(commonFunctions
                .convertManageDefectsToDraftDefects(qn.getDefect())).thenReturn(defects);

        when(commonFunctions
                .convertManageQualitynotificationtoQualityNotifications(qn)).thenReturn(qualityNotifications);
        when(complaintDao.getComplaintBasedOnCode(qn.getComplaintCode())).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(configurationService.validateSupplierContactPerson(qn.getContactPersonCode())).thenReturn(partner);
        when(configurationService.validatePersonResponsibleCode(qn.getPersonResponsibleCode())).thenReturn(partner);

        handler.beforeManageQualityNotificationCreate(qn,createContextMock);
    }


    @Test
    public void testOnManageQualityNotificationCreateNull() {
        when(commonFunctions
                .convertManageQualitynotificationtoQualityNotifications(qn)).thenReturn(qualityNotifications);
        handler.onManageQualityNotificationCreate(qn);
    }

    @Test
    public void testOnManageQualityNotificationCreate() {
        qualityNotifications.setIdentifier("1");
        when(messageService.createComplaintAndQN(qn)).thenReturn(complaints);
        when(commonFunctions
                .convertManageQualitynotificationtoQualityNotifications(qn)).thenReturn(qualityNotifications);
        handler.onManageQualityNotificationCreate(qn);
    }

    @Test
    public void testAfterManageQualityNotificationCreateUpdate() {
        when(commonFunctions
                .convertManageQualitynotificationtoQualityNotifications(qn)).thenReturn(qualityNotifications);
                handler.afterManageQualityNotificationCreateUpdate(qn);
    }


    @Test
    public void testOnManageQualityNotificationUpdate(){
        when(commonFunctions
        .convertManageQualitynotificationtoQualityNotifications(qn)).thenReturn(qualityNotifications);
        handler.onManageQualityNotificationUpdate(qn);
    }

    @Test
    public void testAfterManageQualityNotificationUpdate(){
        when(commonFunctions
        .convertManageQualitynotificationtoQualityNotifications(qn)).thenReturn(qualityNotifications);
        handler.afterManageQualityNotificationUpdate(qn);
    }

    @Test
    public void testUpdateQualityNotifications(){
        when(mockDb.run(any(CqnUpdate.class))).thenReturn(result);
        handler.updateQualityNotifications(qn,createContextMock,qn.getId());
    }
    
    @Test
    public void testBeforeManageQualityNotificationUpdate(){
        when(manageQualityNotificationService.getQualityNotificationDetailsFromActive(qn.getId())).thenReturn(qn);
        when(qualityNotificationService.getQualityNotificationDetails(qn.getId())).thenReturn(qualityNotifications);
        when(qualityNotificationService.getDefectBasedOnQN(qn.getId())).thenReturn(defects);
        when(manageQualityNotificationService.updateQualityNotification(qn)).thenReturn(qn);
        handler.beforeManageQualityNotificationUpdate(qn);

    }


}