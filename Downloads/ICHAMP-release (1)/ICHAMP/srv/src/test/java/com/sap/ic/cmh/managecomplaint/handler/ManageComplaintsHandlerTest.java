package com.sap.ic.cmh.managecomplaint.handler;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.ServiceException;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.auditlog.AuditLogHelper;
import com.sap.ic.cmh.auditlog.AuditLogDifference;
import com.sap.ic.cmh.claim.service.ClaimService;
import com.sap.ic.cmh.complaint.handler.ComplaintHandler;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.validation.ComplaintValidation;
import com.sap.ic.cmh.configuration.model.MasterData;
import com.sap.ic.cmh.configuration.service.ConfigurationService;
import com.sap.ic.cmh.costcollector.service.CostCollectorService;
import com.sap.ic.cmh.qualitynotification.service.QualityNotificationService;
import com.sap.ic.cmh.utils.CommonFunctions;

import cds.gen.claimservice.Claims;
import cds.gen.complaintservice.BTPUsers;
import cds.gen.managecomplaintservice.Complaints;
import cds.gen.managecomplaintservice.CostCollectors;
import cds.gen.masterdataservice.BusinessPartners;
import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
import cds.gen.masterdataservice.Plants;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.qualitynotificationservice.QualityNotifications;

public class ManageComplaintsHandlerTest {

    @InjectMocks
    @Autowired
    ManageComplaintsHandler handler;

    @Mock
    ComplaintHandler complaintHandler;

    @Mock
    CommonFunctions commonFunctions;

    @Mock
    ConfigurationService configurationService;

    @Mock
    ComplaintService complaintService;

    @Mock
    ComplaintValidation complaintValidation;

    @Mock
    CostCollectorService costCollectorService;

    @Mock
    Messages messages;
    @Mock
    Message message;
    @Mock
    ComplaintsDao complaintDao;
    @Mock
    Result result;

    @Mock
    AuditLogHelper auditLogHelper;
    
    @Mock
	QualityNotificationService qualityNotificationService;

    @Mock
	ClaimService claimService;
 

    @Mock 
    private AuditLogDifference auditLogDifference;
    
    private Complaints manageComplaint;
    private cds.gen.complaintservice.Complaints complaints;
    private MasterData masterData;
    private BusinessPartners partner;
    private CostCollectors manageCostCollectors;
    private cds.gen.costcollectorservice.CostCollectors costCollectors;
    private MaterialMasterGeneralDatas material;
    private Plants plants;
    private PurchaseOrganizations purchaseOrg;
    private QualityNotifications qn;
    private Claims claims;

    private Row row;
    private Optional<Row> opt;
    private List<cds.gen.complaintservice.Complaints> complaintList= new ArrayList<>();
    private List<cds.gen.managecomplaintservice.Complaints> manageComplaintList= new ArrayList<>();

    @Before
    public void beforeClass() {
        MockitoAnnotations.openMocks(this);

        row = Struct.create(Row.class);

        manageComplaint = Struct.create(Complaints.class);
        manageComplaint.setCompanyCodeId("D445");

        complaints = Struct.create(cds.gen.complaintservice.Complaints.class);
        complaints.setIdentifier("1");
        complaints.setId("11111");
        complaints.setPurchasingOrganizationId("3456");

        complaintList.add(complaints);

        manageComplaint.setCompanyCodeId("D445");
        manageComplaint.setMaterialCode("F56788");
        manageComplaint.setPlantCode("F8776");
        manageComplaint.setSupplierCode("F8776");
        manageComplaint.setPersonResponsibleCode("F8776");
        manageComplaint.setPurchaseOrganisationCode("F8776");
        manageComplaint.setCreationType("Manual");

        plants = Struct.create(Plants.class);
        plants.setId("2");
        plants.setPlant("plant");

        material = Struct.create(MaterialMasterGeneralDatas.class);
        material.setId("1");
        material.setMaterialCode("codeM");

        partner = Struct.create(BusinessPartners.class);
        partner.setBusinessPartnerNumber("P567");
        partner.setId("1234");

        purchaseOrg = Struct.create(PurchaseOrganizations.class);
        purchaseOrg.setId("3456");
        purchaseOrg.setPurchaseOrganization("F8776");

        masterData = new MasterData();
        masterData.setPlants(plants);
        masterData.setMaterial(material);
        masterData.setSupplier(partner);
        masterData.setPersonResponsible(partner);
        masterData.setPurchaseOrg(purchaseOrg);

        manageCostCollectors = Struct.create(CostCollectors.class);
        manageCostCollectors.setQuantity(BigDecimal.TEN);
        manageCostCollectors.setTotalCost(BigDecimal.TEN);
        manageCostCollectors.setComplaintCode("1");
        
         costCollectors = Struct.create(cds.gen.costcollectorservice.CostCollectors.class);
         costCollectors.setQuantity(BigDecimal.TEN);
         costCollectors.setTotalCost(BigDecimal.TEN);
         
         qn = Struct.create(QualityNotifications.class);
         qn.setComplaintId("11111");
         qn.setIdentifier("5");
         claims = Struct.create(Claims.class);
         claims.setComplaintId("11111");
         claims.setIdentifier("5");
         
    }

    @Test
    public void testBeforeManageComplaintCreate() {
        // when(configurationService.getMasterDataDetailsByCodes(manageComplaint.getMaterialCode(),manageComplaint.getPlantCode(),manageComplaint.getSupplierCode(),manageComplaint.getPersonResponsibleCode(),manageComplaint.getPurchaseOrganisationCode())).thenReturn(masterData);
        when(commonFunctions.convertManageComplaintsToComplaints(manageComplaint)).thenReturn(complaints);
        when(configurationService.validateSupplierContactPerson(manageComplaint.getContactPersonCode()))
                .thenReturn(partner);
        when(complaintValidation.validateResponsiblePerson(manageComplaint.getPersonResponsibleCode()))
                .thenReturn(true);
        handler.beforeManageComplaintCreate(manageComplaint);
    }

    @Test
    public void testBeforeManageComplaintCreateMasterDataNull() {
        MasterData masterData1 = new MasterData();
        // when(configurationService.getMasterDataDetailsByCodes(manageComplaint.getMaterialCode(),manageComplaint.getPlantCode(),manageComplaint.getSupplierCode(),manageComplaint.getPersonResponsibleCode(),manageComplaint.getPurchaseOrganisationCode())).thenReturn(masterData1);
        when(commonFunctions.convertManageComplaintsToComplaints(manageComplaint)).thenReturn(complaints);
        handler.beforeManageComplaintCreate(manageComplaint);
    }

    @Test
    public void testOnManageComplaintsCreate() {
        when(commonFunctions.convertManageComplaintsToComplaints(manageComplaint)).thenReturn(complaints);
        handler.onManageComplaintsCreate(manageComplaint);
    }

    @Test
    public void testAfterManageComplaintUpdate() {
        when(commonFunctions.convertManageComplaintsToComplaints(manageComplaint)).thenReturn(complaints);
        when(complaintService.getComplaintDetails(any())).thenReturn(complaints);
        handler.afterManageComplaintUpdate(manageComplaint);
    }

    @Test
    public void testAfterManageComplaintCreate() {
        when(complaintService.getComplaintDetails(any())).thenReturn(complaints);
        handler.afterManageComplaintsCreate(manageComplaint);
    }

    @Test
    public void testAfterCostCollectorCreate() {
        when(commonFunctions.convertManageComplaintsToComplaints(manageComplaint)).thenReturn(complaints);
        when(complaintDao.getComplaintBasedOnCode(manageCostCollectors.getComplaintCode())).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(costCollectorService.calculateExchangeRateAndConvertCost(any(), any())).thenReturn(BigDecimal.TEN);
        handler.onCostCollectorCreate(manageCostCollectors);
    }

    @Test
    public void testAfterCostCollectorCreateConvertedCostNull() {
        when(commonFunctions.convertManageComplaintsToComplaints(manageComplaint)).thenReturn(complaints);
        when(complaintDao.getComplaintBasedOnCode(manageCostCollectors.getComplaintCode())).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(costCollectorService.calculateExchangeRateAndConvertCost(any(), any())).thenReturn(null);
        handler.onCostCollectorCreate(manageCostCollectors);
    }

    @Test
    public void testAfterCostCollectorCreateTotalCostNull() {
        manageCostCollectors.setTotalCost(null);
        when(commonFunctions.convertManageComplaintsToComplaints(manageComplaint)).thenReturn(complaints);
        when(complaintDao.getComplaintBasedOnCode(manageCostCollectors.getComplaintCode())).thenReturn(result);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ComplaintID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        handler.onCostCollectorCreate(manageCostCollectors);
    }

    @Test
    public void testAfterCostCollectorCreateTotalCostAndQuantityNull() {
        manageCostCollectors.setTotalCost(null);
        manageCostCollectors.setQuantity(null);
        when(commonFunctions.convertManageCostCollectorToCostCollector(manageCostCollectors)).thenReturn(costCollectors);
//        when(complaintDao.getComplaintBasedOnCode(manageCostCollectors.getComplaintCode())).thenReturn(result);
//        List<Row> rowvalues = new ArrayList<>();
//        row.put("ID", "ComplaintID");
//        opt = Optional.of(row);
//        rowvalues.add(row);
//        when(result.list()).thenReturn(rowvalues);
//        when(result.first()).thenReturn(opt);
        handler.onCostCollectorCreate(manageCostCollectors);
    }

    @Test(expected = ServiceException.class)
    public void testBeforeCostCollectorCreateServiceException() {
        manageCostCollectors.setTotalCost(null);
        manageCostCollectors.setQuantity(null);
        manageCostCollectors.setComplaintCode(null);
        when(commonFunctions.convertManageCostCollectorToCostCollector(manageCostCollectors)).thenReturn(costCollectors);
        when(complaintDao.getComplaintBasedOnCode(manageCostCollectors.getComplaintCode())).thenReturn(null);
        handler.beforeCostCollectorCreate(manageCostCollectors);
    }
    @Test
    public void testBeforeCostCollectorCreate() {
        manageCostCollectors.setComplaintCode("1");
        when(complaintDao.getComplaintBasedOnCode(manageCostCollectors.getComplaintCode())).thenReturn(result);
        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId("1111")).thenReturn(null);
        when(claimService.getClaim("1111")).thenReturn(claims);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "1111");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        handler.beforeCostCollectorCreate(manageCostCollectors);
    }
    
    @Test
    public void testBeforeCostCollectorCreateClaimNull() {
        manageCostCollectors.setComplaintCode("1");
        when(complaintDao.getComplaintBasedOnCode(manageCostCollectors.getComplaintCode())).thenReturn(result);
        when(qualityNotificationService.getQualityNotificationDetailsByComplaintId("1111")).thenReturn(qn);
        when(claimService.getClaim("1111")).thenReturn(null);
        List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "1111");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        Mockito.when(messages.error(any(String.class), any(Object[].class))).thenReturn(message);
        handler.beforeCostCollectorCreate(manageCostCollectors);
    }

    @Test
    public void testbeforeComplaintUpdate() {
        manageComplaint.setComplaintStatusCode("CRTD");
        manageComplaint.setId("11111");
        // when(configurationService.getMasterDataDetailsByCodes(manageComplaint.getMaterialCode(),manageComplaint.getPlantCode(),manageComplaint.getSupplierCode(),manageComplaint.getPersonResponsibleCode(),manageComplaint.getPurchaseOrganisationCode())).thenReturn(masterData);
        when(complaintDao.getComplaintDetails(manageComplaint.getId())).thenReturn(result);
        when(result.listOf(cds.gen.complaintservice.Complaints.class)).thenReturn(complaintList);
        when(commonFunctions.convertManageComplaintsToComplaints(manageComplaint)).thenReturn(complaints);
        handler.beforeComplaintUpdate(manageComplaint);
    }

    @Test
    public void testbeforeComplaintUpdateManageComplaintNull() {
        manageComplaint.setComplaintStatusCode("CRTD");
        manageComplaint.setId("11111");
        manageComplaint.setMaterialCode(null);
        manageComplaint.setPlantCode(null);
        manageComplaint.setSupplierCode(null);
        manageComplaint.setPersonResponsibleCode(null);
        manageComplaint.setPurchaseOrganisationCode(null);
        when(complaintDao.getComplaintDetails(manageComplaint.getId())).thenReturn(result);
        when(result.listOf(cds.gen.complaintservice.Complaints.class)).thenReturn(complaintList);
        when(commonFunctions.convertManageComplaintsToComplaints(manageComplaint)).thenReturn(complaints);
     
        handler.beforeComplaintUpdate(manageComplaint);
    }

    @Test
    public void testbeforeComplaintUpdateStatusDiff() {
        manageComplaint.setComplaintStatusCode("ACRTD");
        manageComplaint.setId("11111");
         when(complaintDao.getComplaintDetails(manageComplaint.getId())).thenReturn(result);
        when(result.listOf(cds.gen.complaintservice.Complaints.class)).thenReturn(complaintList);
        when(commonFunctions.convertManageComplaintsToComplaints(manageComplaint)).thenReturn(complaints);
        // when(configurationService.getMasterDataDetailsByCodes(manageComplaint.getMaterialCode(),manageComplaint.getPlantCode(),manageComplaint.getSupplierCode(),manageComplaint.getPersonResponsibleCode(),manageComplaint.getPurchaseOrganisationCode())).thenReturn(masterData);
        handler.beforeComplaintUpdate(manageComplaint);
    }

    @Test
    public void testbeforeComplaintUpdatenull() {
        manageComplaint.setComplaintStatusCode(null);
        manageComplaint.setCreationType(null);
        manageComplaint.setId("11111");
        when(complaintDao.getComplaintDetails(manageComplaint.getId())).thenReturn(result);
        when(result.listOf(cds.gen.complaintservice.Complaints.class)).thenReturn(complaintList);
        when(commonFunctions.convertManageComplaintsToComplaints(manageComplaint)).thenReturn(complaints);
        // when(configurationService.getMasterDataDetailsByCodes(manageComplaint.getMaterialCode(),manageComplaint.getPlantCode(),manageComplaint.getSupplierCode(),manageComplaint.getPersonResponsibleCode(),manageComplaint.getPurchaseOrganisationCode())).thenReturn(masterData);
        handler.beforeComplaintUpdate(manageComplaint);
    }

    @Test
    public void testbeforeComplaintUpdateEmpty() {
        manageComplaint.setComplaintStatusCode(null);
        manageComplaint.setCreationType("Automatic1");
        when(commonFunctions.convertManageComplaintsToComplaints(manageComplaint)).thenReturn(complaints);
        // when(configurationService.getMasterDataDetailsByCodes(manageComplaint.getMaterialCode(),manageComplaint.getPlantCode(),manageComplaint.getSupplierCode(),manageComplaint.getPersonResponsibleCode(),manageComplaint.getPurchaseOrganisationCode())).thenReturn(masterData);
        handler.beforeComplaintUpdate(manageComplaint);
    }

    @Test
    public void testAfterComplaintCreateUpdate() {
        List<Complaints> data = new ArrayList<>();
        data.add(manageComplaint);
        //handler.afterComplaintCreateUpdate(data);
    }

    @Test
    public void testAfterComplaintRead(){
        BTPUsers users=Struct.create(BTPUsers.class);
        users.setPersonResponsibleNumber("abcd");
        users.setPersonResponsibleId("1234");
        List<BTPUsers>userList=new ArrayList();
        userList.add(users);
        manageComplaint=Struct.create(Complaints.class);
        manageComplaint.setId("1234");
        manageComplaint.setComplaintStatusCode("CLSD");
        manageComplaint.setPersonResponsibleId("abcd");
        manageComplaintList.add(manageComplaint);
        when(complaintService.getAllResponsiblePerson()).thenReturn(userList);
        handler.afterComplaintRead(manageComplaintList);
    }

}
