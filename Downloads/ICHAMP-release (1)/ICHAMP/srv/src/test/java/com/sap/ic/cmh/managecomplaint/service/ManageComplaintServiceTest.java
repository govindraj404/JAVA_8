 package com.sap.ic.cmh.managecomplaint.service;

 import cds.gen.managecomplaintservice.Complaints;
 import cds.gen.masterdataservice.BusinessPartners;
 import cds.gen.masterdataservice.MaterialMasterGeneralDatas;
 import cds.gen.masterdataservice.Plants;
 import cds.gen.masterdataservice.PurchaseOrganizations;
 import com.sap.cds.Result;
 import com.sap.cds.Struct;
 import com.sap.cds.ql.cqn.CqnInsert;
 import com.sap.cds.ql.cqn.CqnUpdate;
 import com.sap.cds.services.cds.CdsService;
 import com.sap.cds.services.messages.Messages;
 import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
 import com.sap.ic.cmh.complaint.service.ComplaintService;
 import com.sap.ic.cmh.configuration.model.MasterData;
 import com.sap.ic.cmh.configuration.service.ConfigurationService;
 import com.sap.ic.cmh.qualitynotification.model.QualityNotificationDetails;
 import com.sap.ic.cmh.qualitynotification.model.SupplierPersonDetails;
 import com.sap.ic.cmh.utils.CommonFunctions;
 import org.junit.Before;
 import org.junit.Test;
 import org.mockito.InjectMocks;
 import org.mockito.Mock;
 import org.mockito.MockitoAnnotations;
 import org.springframework.beans.factory.annotation.Autowired;

 import java.math.BigDecimal;
 import java.util.ArrayList;
 import java.util.List;

 import static org.mockito.ArgumentMatchers.any;
 import static org.mockito.Mockito.when;
 import cds.gen.managequalitynotificationservice.QualityNotifications;

 public class ManageComplaintServiceTest {

     @InjectMocks
     @Autowired
     ManageComplaintService service;

     @Mock
     CdsService cdsService;

     @Mock
     ComplaintService complaintService;

     @Mock
     ConfigurationService configService;

     @Mock
     Messages messages;

     @Mock
     CommonFunctions commonFunctions;

     @Mock
     ComplaintsDao complaintDao;

     @Mock
     Result result;


     private QualityNotifications qn;

     private QualityNotificationDetails qualityNotificationDetails;

     private MasterData masterDataDetailsByCodes;

     private BusinessPartners partner;

     private List<Complaints> list;

     private Complaints complaints;

     private cds.gen.complaintservice.Complaints manageComplaint;
     private SupplierPersonDetails supplierPersonDetails;

     @Before
     public void beforeClass() {
         MockitoAnnotations.openMocks(this);

         qn = Struct.create(QualityNotifications.class);
         qn.setMaterialId("1234");
         qn.setSupplierId("1234");
         qn.setPlantId("1234");
         qn.setPurchasingOrganizationId("3456");
         qn.setQuantity(BigDecimal.TEN);
         qn.setComplaintId("1234");


         qualityNotificationDetails = new QualityNotificationDetails();
         qualityNotificationDetails.setQuantity("223");
         qualityNotificationDetails.setMaterial("SDE");
         qualityNotificationDetails.setPlant("plant");
         qualityNotificationDetails.setSupplier("supplier");
         qualityNotificationDetails.setPersonResponsible("C5327163");
         qualityNotificationDetails.setReferenceNumber("CMH-REF");

         Plants plants = Struct.create(Plants.class);
         plants.setPlant("plant");
         plants.setId("1234");

         MaterialMasterGeneralDatas material = Struct.create(MaterialMasterGeneralDatas.class);
         material.setMaterialCode("codeM");
         material.setId("1234");

         partner = Struct.create(BusinessPartners.class);
         partner.setBusinessPartnerNumber("P567");
         partner.setId("1234");

         PurchaseOrganizations purchaseOrg = Struct.create(PurchaseOrganizations.class);
         purchaseOrg.setId("3456");




         masterDataDetailsByCodes = new MasterData();
         masterDataDetailsByCodes.setPlants(plants);
         masterDataDetailsByCodes.setMaterial(material);
         masterDataDetailsByCodes.setSupplier(partner);
         masterDataDetailsByCodes.setPersonResponsible(partner);
         masterDataDetailsByCodes.setPurchaseOrg(purchaseOrg);

         complaints = Struct.create(Complaints.class);
         complaints.setCompanyCodeId("G6677");
         complaints.setMaterialCode("F56788");
         complaints.setPlantCode("F8776");
         complaints.setSupplierCode("F8776");
         complaints.setPersonResponsibleCode("F8776");
         complaints.setPurchaseOrganisationCode("F8776");
         complaints.setQuantity(BigDecimal.TEN);

         manageComplaint = Struct.create(cds.gen.complaintservice.Complaints.class);
         manageComplaint.setCompanyCodeId("D445");
         manageComplaint.setComplaintTypeCode("F877");
         manageComplaint.setId("1234");
         manageComplaint.setMaterial(material);
         manageComplaint.setQuantity(BigDecimal.TEN);

         list = new ArrayList<>();
         list.add(complaints);
     }

     @Test
     public void testCreateComplaintAutomatic() {

         when(cdsService.run(any(CqnInsert.class))).thenReturn(result);
         when(result.listOf(Complaints.class)).thenReturn(list);
         // String tenentId = "FGHJ";
         service.createComplaintAutomatic(qn);
     }


     @Test
     public void testUpdateMasterDataForComplaint() {
         when(complaintService.getComplaintDetails("1234")).thenReturn(manageComplaint);
         when(commonFunctions.convertComplaintToManageComplaint(manageComplaint)).thenReturn(complaints);
         when(cdsService.run(any(CqnUpdate.class))).thenReturn(result);
         when(result.listOf(Complaints.class)).thenReturn(list);
         service.updateMasterDataForComplaint("1234",qualityNotificationDetails, BigDecimal.TEN,"KG");
     }

     @Test
     public void testUpdateMasterDataForComplaints(){
         when(complaintService.getComplaintDetails("1234")).thenReturn(manageComplaint);
         when(commonFunctions.convertComplaintToManageComplaint(manageComplaint)).thenReturn(complaints);

         when(cdsService.run(any(CqnUpdate.class))).thenReturn(result);
         when(result.listOf(Complaints.class)).thenReturn(list);
         service.updateMasterDataForComplaints(qn);
     }



 }
