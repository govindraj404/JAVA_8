package com.sap.ic.cmh.complaint.validation;

import cds.gen.complaintservice.BusinessObjects;
import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Streams;
import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.complaint.service.ComplaintService;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.masterdata.common.service.MasterDataService;
import com.sap.ic.cmh.utils.Constants;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.when;

public class StreamValidationImplTest {

	@InjectMocks
	@Autowired
	StreamValidationImpl streamValidator;

	@Mock
	MasterDataService masterDataService;

	@Mock
	ComplaintService complaintService;

	@Mock
	StreamService streamService;

	@Mock
	Messages messages;

	@Mock
	Result result;

	@Mock
	Message message1;

	Streams streams;
	Complaints complaint;
	BusinessObjects eachBo;

	@Before
	public void setup() {
		MockitoAnnotations.openMocks(this);

		streams = Struct.create(Streams.class);
		streams.setStatusCode("N88");
		streams.setId("H099");
		streams.setParentIDId("35556L");
		streams.setIsRelevant(false);
		streams.setStreamTypeCode("QLTY");

		complaint = Struct.create(Complaints.class);
		complaint.setId("ComplaintID");
		complaint.setReferenceNumber("1234");
		complaint.setComplaintStatusCode("CLSD");

		eachBo = Struct.create(BusinessObjects.class);
		eachBo.setBusinessObjectIDId("G889");
		eachBo.setId("G675");
		eachBo.setComplaint("CMPLI");
		eachBo.setIsRelevant(false);
	}

	@Test
	public void testValidateStreamEdit() {
		when(streamService.getStream(streams.getId())).thenReturn(result);
		when(result.single(Streams.class)).thenReturn(streams);
		when(complaintService.getComplaintDetails(streams.getParentIDId())).thenReturn(complaint);
		when(messages.error(MessageKeys.MANDATORY_STREAM)).thenReturn(message1);
		streamValidator.validateStreamEdit(streams);
	}

	@Test
	public void testValidateStreamEditStatusCode() {
		complaint.setComplaintStatusCode("NEW");
		when(streamService.getStream(streams.getId())).thenReturn(result);
		when(result.single(Streams.class)).thenReturn(streams);
		when(complaintService.getComplaintDetails(streams.getParentIDId())).thenReturn(complaint);
		when(messages.error(MessageKeys.MANDATORY_STREAM)).thenReturn(message1);
		streamValidator.validateStreamEdit(streams);
	}

	@Test
	public void testValidateStreamEditStatusCodeNone() {
		complaint.setComplaintStatusCode("NONE");
		when(streamService.getStream(streams.getId())).thenReturn(result);
		when(result.single(Streams.class)).thenReturn(streams);
		when(complaintService.getComplaintDetails(streams.getParentIDId())).thenReturn(complaint);
		when(messages.error(MessageKeys.MANDATORY_STREAM)).thenReturn(message1);
		streamValidator.validateStreamEdit(streams);
	}

	@Test
	public void testValidateStreamEditIsRelevantNull() {
		streams.setIsRelevant(null);
		complaint.setComplaintStatusCode("NONE");
		when(streamService.getStream(streams.getId())).thenReturn(result);
		when(result.single(Streams.class)).thenReturn(streams);
		when(complaintService.getComplaintDetails(streams.getParentIDId())).thenReturn(complaint);
		when(messages.error(MessageKeys.MANDATORY_STREAM)).thenReturn(message1);
		streamValidator.validateStreamEdit(streams);
	}

	@Test
	public void testValidateStreamEditIsRelevantTrue() {
		streams.setIsRelevant(true);
		complaint.setComplaintStatusCode("NONE");
		when(streamService.getStream(streams.getId())).thenReturn(result);
		when(result.single(Streams.class)).thenReturn(streams);
		when(complaintService.getComplaintDetails(streams.getParentIDId())).thenReturn(complaint);
		when(messages.error(MessageKeys.MANDATORY_STREAM)).thenReturn(message1);
		streamValidator.validateStreamEdit(streams);
	}

	@Test
	public void testValidateStreamEdit2() {
		streams.setStreamTypeCode(Constants.CLAIM_CODE);
		List<BusinessObjects> listBo = new ArrayList<>();
		listBo.add(eachBo);
		streams.setBusinessObjects(listBo);
		when(streamService.getStream(streams.getId())).thenReturn(result);
		when(result.single(Streams.class)).thenReturn(streams);
		when(complaintService.getComplaintDetails(streams.getParentIDId())).thenReturn(complaint);
		when(messages.error(MessageKeys.BUSINESS_OBJECT_IS_CREATED)).thenReturn(message1);
		streamValidator.validateStreamEdit(streams);
	}

	@Test
	public void testValidateStreamEditSetBusinessObjectNull() {
		streams.setStreamTypeCode(Constants.CLAIM_CODE);
		streams.setBusinessObjects(null);
		when(streamService.getStream(streams.getId())).thenReturn(result);
		when(result.single(Streams.class)).thenReturn(streams);
		when(complaintService.getComplaintDetails(streams.getParentIDId())).thenReturn(complaint);
		when(messages.error(MessageKeys.BUSINESS_OBJECT_IS_CREATED)).thenReturn(message1);
		streamValidator.validateStreamEdit(streams);
	}

	@Test
	public void testValidateStreamEditBusinessObjectNull() {
		streams.setStreamTypeCode(Constants.CLAIM_CODE);
		List<BusinessObjects> listBo = new ArrayList<>();
		listBo.add(0,Struct.create(BusinessObjects.class));
		streams.setBusinessObjects(listBo);
		when(streamService.getStream(streams.getId())).thenReturn(result);
		when(result.single(Streams.class)).thenReturn(streams);
		when(complaintService.getComplaintDetails(streams.getParentIDId())).thenReturn(complaint);
		when(messages.error(MessageKeys.BUSINESS_OBJECT_IS_CREATED)).thenReturn(message1);
		streamValidator.validateStreamEdit(streams);
	}

	@Test
	public void testValidateStreamEditCompanyCodeNull() {
		complaint.setComplaintStatusCode(null);
		when(streamService.getStream(streams.getId())).thenReturn(result);
		when(result.single(Streams.class)).thenReturn(streams);
		when(complaintService.getComplaintDetails(streams.getParentIDId())).thenReturn(complaint);
		when(messages.error(MessageKeys.MANDATORY_STREAM)).thenReturn(message1);
		when(messages.error(MessageKeys.INVALID_COMPLAINT_STATE)).thenReturn(message1);
		streamValidator.validateStreamEdit(streams);
	}

	@Test
	public void testValidateStreamEditForNEW() {
		Map<String, String> map = new HashMap<>();
		map.put("STREAM_ID", "1234");
		map.put("ID", "1234");
		List<Map<String, ?>> lists = new ArrayList<>();
		lists.add(map);
		streams.setBusinessObjects(lists);
		streams.setStreamTypeCode("NEW");
		when(streamService.getStream(streams.getId())).thenReturn(result);
		when(result.single(Streams.class)).thenReturn(streams);
		when(complaintService.getComplaintDetails(streams.getParentIDId())).thenReturn(complaint);
		when(messages.error(MessageKeys.MANDATORY_STREAM)).thenReturn(message1);
		when(messages.error(MessageKeys.BUSINESS_OBJECT_IS_CREATED)).thenReturn(message1);
		streamValidator.validateStreamEdit(streams);
	}

	@Test
	public void testValidateBusinessObjectsEdit() {
		eachBo.setBusinessObjectTypeCode("QN");
		when(streamService.getBusinessObject(eachBo.getId())).thenReturn(result);
		when(result.single(BusinessObjects.class)).thenReturn(eachBo);
		when(complaintService.getComplaintDetails(eachBo.getComplaint())).thenReturn(complaint);
		when(messages.error(MessageKeys.MANDATORY_BUSINESSOBJECT)).thenReturn(message1);
		streamValidator.validateBusinessObjectsEdit(eachBo);
	}

	@Test
	public void testValidateBusinessObjectsEditIsRelevantTrue() {
		eachBo.setBusinessObjectTypeCode("QN");
		eachBo.setIsRelevant(true);
		when(streamService.getBusinessObject(eachBo.getId())).thenReturn(result);
		when(result.single(BusinessObjects.class)).thenReturn(eachBo);
		when(complaintService.getComplaintDetails(eachBo.getComplaint())).thenReturn(complaint);
		when(messages.error(MessageKeys.MANDATORY_BUSINESSOBJECT)).thenReturn(message1);
		streamValidator.validateBusinessObjectsEdit(eachBo);
	}

	@Test
	public void testValidateBusinessObjectsEditIsRelevantNull() {
		eachBo.setBusinessObjectTypeCode("QN");
		eachBo.setIsRelevant(null);
		when(streamService.getBusinessObject(eachBo.getId())).thenReturn(result);
		when(result.single(BusinessObjects.class)).thenReturn(eachBo);
		when(complaintService.getComplaintDetails(eachBo.getComplaint())).thenReturn(complaint);
		when(messages.error(MessageKeys.MANDATORY_BUSINESSOBJECT)).thenReturn(message1);
		streamValidator.validateBusinessObjectsEdit(eachBo);
	}

	@Test
	public void testValidateBusinessObjectsEditCompanyStatusCodeNull() {
		complaint.setComplaintStatusCode(null);
		eachBo.setBusinessObjectTypeCode("QN");
		when(streamService.getBusinessObject(eachBo.getId())).thenReturn(result);
		when(result.single(BusinessObjects.class)).thenReturn(eachBo);
		when(complaintService.getComplaintDetails(eachBo.getComplaint())).thenReturn(complaint);
		when(messages.error(MessageKeys.MANDATORY_BUSINESSOBJECT)).thenReturn(message1);
		streamValidator.validateBusinessObjectsEdit(eachBo);
	}

	@Test
	public void testValidateBusinessObjectsEditBusinessObjectIDIdNull() {
		eachBo.setBusinessObjectTypeCode("NO");
		eachBo.setBusinessObjectIDId(null);
		when(streamService.getBusinessObject(eachBo.getId())).thenReturn(result);
		when(result.single(BusinessObjects.class)).thenReturn(eachBo);
		when(complaintService.getComplaintDetails(eachBo.getComplaint())).thenReturn(complaint);
		when(messages.error(MessageKeys.MANDATORY_BUSINESSOBJECT)).thenReturn(message1);
		streamValidator.validateBusinessObjectsEdit(eachBo);
	}

	@Test
	public void testValidateBusinessObjectsEditNull() {
		eachBo.setBusinessObjectTypeCode("QN");
		eachBo.setComplaint(null);
		when(streamService.getBusinessObject(eachBo.getId())).thenReturn(result);
		when(result.single(BusinessObjects.class)).thenReturn(eachBo);
		when(complaintService.getComplaintDetails(eachBo.getComplaint())).thenReturn(complaint);
		when(messages.error(MessageKeys.MANDATORY_BUSINESSOBJECT)).thenReturn(message1);
		streamValidator.validateBusinessObjectsEdit(eachBo);
	}

	@Test
	public void testValidateBusinessObjectsEditForTypeCodeNOTQN() {
		eachBo.setBusinessObjectTypeCode("LT");
		when(streamService.getBusinessObject(eachBo.getId())).thenReturn(result);
		when(result.single(BusinessObjects.class)).thenReturn(eachBo);
		when(complaintService.getComplaintDetails(eachBo.getComplaint())).thenReturn(complaint);
		when(messages.error(MessageKeys.BUSINESS_OBJECT_IS_CREATED)).thenReturn(message1);
		streamValidator.validateBusinessObjectsEdit(eachBo);
	}

}
