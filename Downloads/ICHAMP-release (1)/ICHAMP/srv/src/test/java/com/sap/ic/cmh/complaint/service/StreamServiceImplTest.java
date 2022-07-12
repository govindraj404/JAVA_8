package com.sap.ic.cmh.complaint.service;

import cds.gen.com.sap.ic.cmh.businessobjecttype.BusinessObjectTypes;
import cds.gen.com.sap.ic.cmh.claimprocessingstreamstatus.ClaimProcessingStreamStatuses;
import cds.gen.com.sap.ic.cmh.complainttypestreamtypemapping.ComplaintTypeStreamTypeMappings;
import cds.gen.com.sap.ic.cmh.streamtype.StreamTypes;
import cds.gen.complaintservice.BusinessObjects;
import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Streams;
import cds.gen.returnpurchaseorderservice.LogisticsStreamStatuses;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;
import com.sap.ic.cmh.businessobjects.service.BusinessObjectService;
import com.sap.ic.cmh.complaint.persistency.StreamDao;
import com.sap.ic.cmh.utils.Constants;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class StreamServiceImplTest {
	@InjectMocks
	@Autowired
	StreamServiceImpl impl;
	@Mock
	BusinessObjectService businessObjectService;
	@Mock
	StreamDao streamDao;
	@Mock
	Result result;
	@Mock
	PersistenceService db;
	@Mock
	ComplaintService complaintService;

	@Before
	public void before() {
		MockitoAnnotations.openMocks(this);
	}

	@Test
	public void createAllStreamsTest() {
		StreamTypes status = Struct.create(StreamTypes.class);
		status.setCode("1");
		status.setName("8");
		ComplaintTypeStreamTypeMappings mapping = Struct.create(ComplaintTypeStreamTypeMappings.class);
		mapping.setComplaintTypeCode("SREC");
		mapping.setStreamTypeCode("1");
		mapping.setSequenceNumber(2);
		when(streamDao.getStreamType()).thenReturn(result);
		when(streamDao.getBusinessObjectTypes()).thenReturn(result);
		when(streamDao.getSequenceBasedOnComplaintStreamType(status.getCode(),"SREC")).thenReturn(result);
		Optional<StreamTypes> emptyOpt = Optional.of(status);
		when(db.run(any(CqnInsert.class))).thenReturn(result);
		when(result.first(StreamTypes.class)).thenReturn(emptyOpt);
		Optional<ComplaintTypeStreamTypeMappings> complaintTypeStreamTypeMappings = Optional.of(mapping);
		when(result.first(ComplaintTypeStreamTypeMappings.class)).thenReturn(complaintTypeStreamTypeMappings);
		impl.createAllStreams(status.getCode(),"SREC");
	}

	@Test
	public void createAllStreamsTest2() {
		StreamTypes status = Struct.create(StreamTypes.class);
		status.setCode("1");
		status.setName("8");
		ComplaintTypeStreamTypeMappings mapping = Struct.create(ComplaintTypeStreamTypeMappings.class);
		mapping.setComplaintTypeCode("SREC");
		mapping.setStreamTypeCode("1");
		mapping.setSequenceNumber(2);
		when(streamDao.getBusinessObjectTypes()).thenReturn(result);
		when(streamDao.getStreamType()).thenReturn(result);
	
		StreamTypes sTypes = Struct.create(StreamTypes.class);
		sTypes.setCode("CLM");
		List<StreamTypes> listSTypes = new ArrayList<>();
		listSTypes.add(sTypes);
		when(result.listOf(StreamTypes.class)).thenReturn(listSTypes);
		when(streamDao.getSequenceBasedOnComplaintStreamType("CLM","SREC")).thenReturn(result);
		Optional<ComplaintTypeStreamTypeMappings> complaintTypeStreamTypeMappings = Optional.of(mapping);
		when(result.first(ComplaintTypeStreamTypeMappings.class)).thenReturn(complaintTypeStreamTypeMappings);
		BusinessObjectTypes bTypes = Struct.create(BusinessObjectTypes.class);
		bTypes.setCode("CL");
		bTypes.setStreamTypeCode("CLM");
		List<BusinessObjectTypes> listBTypes = new ArrayList<>();
		listBTypes.add(bTypes);
		when(result.listOf(BusinessObjectTypes.class)).thenReturn(listBTypes);
		when(streamDao.createStream(any(Streams.class))).thenReturn(result);
		Streams stream = Struct.create(Streams.class);
		stream.setId("ID");
		when(result.single(Streams.class)).thenReturn(stream);

		when(streamDao.insertBusinessObjects(any(BusinessObjects.class)))
				.thenReturn(Struct.create(BusinessObjects.class));

		when(db.run(any(CqnInsert.class))).thenReturn(result);
		
		impl.createAllStreams(status.getCode(),"SREC");
	}

	@Test
	public void testCreateAllStreamsException() {
		StreamTypes status = Struct.create(StreamTypes.class);
		status.setCode("1");
		status.setName("8");
		ComplaintTypeStreamTypeMappings mapping = Struct.create(ComplaintTypeStreamTypeMappings.class);
		mapping.setComplaintTypeCode("SREC");
		mapping.setStreamTypeCode("1");
		mapping.setSequenceNumber(2);
		when(streamDao.getBusinessObjectTypes()).thenReturn(result);
		when(streamDao.getStreamType()).thenReturn(result);
		when(streamDao.getSequenceBasedOnComplaintStreamType("CLM", "SREC")).thenReturn(result);
		Optional<ComplaintTypeStreamTypeMappings> complaintTypeStreamTypeMappings = Optional.of(mapping);
		when(result.first(ComplaintTypeStreamTypeMappings.class)).thenReturn(complaintTypeStreamTypeMappings);
		StreamTypes sTypes = Struct.create(StreamTypes.class);
		sTypes.setCode("CLM");
		List<StreamTypes> listSTypes = new ArrayList<>();
		listSTypes.add(sTypes);
		when(result.listOf(StreamTypes.class)).thenReturn(listSTypes);
		BusinessObjectTypes bTypes = Struct.create(BusinessObjectTypes.class);
		bTypes.setCode("CL");
		bTypes.setStreamTypeCode("CLM");
		List<BusinessObjectTypes> listBTypes = new ArrayList<>();
		listBTypes.add(bTypes);
		when(result.listOf(BusinessObjectTypes.class)).thenReturn(listBTypes);
		when(streamDao.createStream(any(Streams.class))).thenThrow(ODataException.class);
		Streams stream = Struct.create(Streams.class);
		stream.setId("ID");
		when(result.single(Streams.class)).thenReturn(stream);

		when(streamDao.insertBusinessObjects(any(BusinessObjects.class)))
				.thenReturn(Struct.create(BusinessObjects.class));

		when(db.run(any(CqnInsert.class))).thenReturn(result);
		impl.createAllStreams(status.getCode(),"SREC");
	}


	@Test
	public void testCreateAllStreamsException2() {
		StreamTypes status = Struct.create(StreamTypes.class);
		status.setCode("1");
		status.setName("8");
		ComplaintTypeStreamTypeMappings mapping = Struct.create(ComplaintTypeStreamTypeMappings.class);
		mapping.setComplaintTypeCode("SREC");
		mapping.setStreamTypeCode("1");
		mapping.setSequenceNumber(0);
		when(streamDao.getBusinessObjectTypes()).thenReturn(result);
		when(streamDao.getStreamType()).thenReturn(result);
		when(streamDao.getSequenceBasedOnComplaintStreamType("CLM", "SREC")).thenReturn(result);
		Optional<ComplaintTypeStreamTypeMappings> complaintTypeStreamTypeMappings = Optional.of(mapping);
		when(result.first(ComplaintTypeStreamTypeMappings.class)).thenReturn(complaintTypeStreamTypeMappings);
		StreamTypes sTypes = Struct.create(StreamTypes.class);
		sTypes.setCode("CLM");
		List<StreamTypes> listSTypes = new ArrayList<>();
		listSTypes.add(sTypes);
		when(result.listOf(StreamTypes.class)).thenReturn(listSTypes);
		BusinessObjectTypes bTypes = Struct.create(BusinessObjectTypes.class);
		bTypes.setCode("CL");
		bTypes.setStreamTypeCode("CLM");
		List<BusinessObjectTypes> listBTypes = new ArrayList<>();
		listBTypes.add(bTypes);
		when(result.listOf(BusinessObjectTypes.class)).thenReturn(listBTypes);
		when(streamDao.createStream(any(Streams.class))).thenReturn(result);
		Streams stream = Struct.create(Streams.class);
		stream.setId("ID");
		when(result.single(Streams.class)).thenReturn(stream);

		when(streamDao.insertBusinessObjects(any(BusinessObjects.class)))
				.thenThrow(ODataException.class);

		when(db.run(any(CqnInsert.class))).thenReturn(result);
		impl.createAllStreams(status.getCode(),"SREC");
	}

	@Test
	public void getBusinessObjectsWithStatusTest() {
		StreamTypes status = Struct.create(StreamTypes.class);
		status.setCode("1");
		status.setName("8");
		impl.getBusinessObjectsWithStatus(status.getCode(), status.getName(), status.getName());
	}

	@Test
	public void getStreamTest() {
		StreamTypes status = Struct.create(StreamTypes.class);
		status.setCode("1");
		status.setName("8");
		impl.getStream(status.getCode());
	}

	@Test(expected = Exception.class)
	public void updateStreamStatusTest() {
		StreamTypes status = Struct.create(StreamTypes.class);
		status.setCode("1");
		status.setName("8");
		impl.updateStreamStatus(status.getCode(), status.getName(), true);
	}

	@Test(expected = Exception.class)
	public void getBusinessObjectsBasedOnBusinessObjectIdTest() {
		BusinessObjects status = Struct.create(BusinessObjects.class);
		status.setBusinessObjectIDId("1");
		status.setId("8");
		when(businessObjectService.getBusinessObjectsBasedOnBusinessObjectId(status.getId())).thenReturn(status);
		impl.getBusinessObjectsBasedOnBusinessObjectId(status.getBusinessObjectIDId(), status.getId(), true);
	}

	@Test
	public void getCurrentBOStatusTest() {
		Streams streams = Struct.create(Streams.class);
		streams.setParentIDId("111");
		streams.setId("112");
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setId("111");
		complaints.setComplaintStatusCode("RVSD");
		when(businessObjectService.getCurrentActiveBOStatus("1111")).thenReturn("3");
		when(streamDao.checkAllStreamStatus(streams.getParentIDId())).thenReturn(result);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		impl.getCurrentBOStatus(streams.getParentIDId(), "", streams.getId(), "1111", true);
	}

	@Test
	public void getCurrentBOStatusFalseTest() {
		Streams streams = Struct.create(Streams.class);
		streams.setParentIDId("111");
		streams.setId("112");
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setId("111");
		complaints.setComplaintStatusCode("RVSD");
		when(businessObjectService.getCurrentBOStatus("1111")).thenReturn("3");
		when(streamDao.checkAllStreamStatus(streams.getParentIDId())).thenReturn(result);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		impl.getCurrentBOStatus(streams.getParentIDId(), "", streams.getId(), "1111", false);
	}

	@Test
	public void getCurrentBOStatusCLMTest() {
		Streams streams = Struct.create(Streams.class);
		streams.setParentIDId("111");
		streams.setId("112");
        Complaints complaints = Struct.create(Complaints.class);
        complaints.setId("111");
        complaints.setComplaintStatusCode("INPR");
		when(businessObjectService.getCurrentBOStatus(any(String.class))).thenReturn("3");
		ClaimProcessingStreamStatuses claimProcessingStreamStatuses = Struct
				.create(ClaimProcessingStreamStatuses.class);
		claimProcessingStreamStatuses.setClaimStatusCode("201");
		Optional<ClaimProcessingStreamStatuses> emptyOpt = Optional.of(claimProcessingStreamStatuses);
		when(db.run(any(CqnInsert.class))).thenReturn(result);
		when(result.first(ClaimProcessingStreamStatuses.class)).thenReturn(emptyOpt);
		when(streamDao.findClaimStreamStatus(any(String.class))).thenReturn(result);
		when(streamDao.checkAllStreamStatus(streams.getParentIDId())).thenReturn(result);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		impl.getCurrentBOStatus(complaints.getId(), "CLM", streams.getId(), "1111", false);
	}

	@Test
	public void getCurrentBOStatusS8DTest() {
		Streams streams = Struct.create(Streams.class);
		streams.setParentIDId("111");
		streams.setId("112");
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setId("111");
		complaints.setComplaintStatusCode("RVSD");
		when(businessObjectService.getCurrentBOStatus(any(String.class))).thenReturn("3");
		when(businessObjectService.getCurrentBOStatusSupplier8D(any(String.class))).thenReturn("3");
		when(businessObjectService.getBusinessObjectStatusBasedOnType(any(String.class), any(String.class)))
				.thenReturn("test");
		when(streamDao.findQualityStreamStatus(any(String.class), any(Map.class))).thenReturn(result);
		ClaimProcessingStreamStatuses claimProcessingStreamStatuses = Struct
				.create(ClaimProcessingStreamStatuses.class);
		Optional<ClaimProcessingStreamStatuses> emptyOpt = Optional.of(claimProcessingStreamStatuses);
		when(db.run(any(CqnInsert.class))).thenReturn(result);
		when(result.first(ClaimProcessingStreamStatuses.class)).thenReturn(emptyOpt);
		when(streamDao.checkAllStreamStatus(streams.getParentIDId())).thenReturn(result);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		impl.getCurrentBOStatus(streams.getParentIDId(), "S8D", streams.getId(), "1111", false);
	}

	@Test
	public void getCurrentBOStatusRPOTest() {
		Streams streams = Struct.create(Streams.class);
		streams.setParentIDId("111");
		streams.setId("112");
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setId("111");
		complaints.setComplaintStatusCode("INPR");
		when(businessObjectService.getCurrentBOStatus(any(String.class))).thenReturn("3");
		when(businessObjectService.getBusinessObjectStatusBasedOnType(any(String.class), any(String.class)))
				.thenReturn("test");
		when(streamDao.findLogisticsStreamStatus(any(String.class))).thenReturn(result);
		LogisticsStreamStatuses claimProcessingStreamStatuses = Struct.create(LogisticsStreamStatuses.class);
		Optional<LogisticsStreamStatuses> emptyOpt = Optional.of(claimProcessingStreamStatuses);
		when(db.run(any(CqnInsert.class))).thenReturn(result);
		when(result.first(LogisticsStreamStatuses.class)).thenReturn(emptyOpt);
		when(streamDao.findQualityStreamStatus(any(String.class), any(Map.class))).thenReturn(result);
		when(streamDao.checkAllStreamStatus(streams.getParentIDId())).thenReturn(result);
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		impl.getCurrentBOStatus(streams.getParentIDId(), "RPO", streams.getId(), "1111", false);
	}

	@Test
	public void setStreamStatusAndUpdateStreamsTest() {
		Streams streams = Struct.create(Streams.class);
		streams.setStatusCode("1");
		streams.setId("8");
		impl.setStreamStatusAndUpdateStreams(streams.getId(), streams.getStatusCode());
	}

	@Test
	public void getBusinessObjectTest() {
		Streams streams = Struct.create(Streams.class);
		streams.setStatusCode("1");
		streams.setId("8");
		impl.getBusinessObject(streams.getId());
	}

	@Test
	public void updateStreamStatusNullTest() {
		BusinessObjects streams = Struct.create(BusinessObjects.class);
		streams.setBusinessObjectTypeCode("1");
		streams.setId("8");
		streams.setStreamId("3");
		streams.setComplaint("7");
		Complaints complaints = Struct.create(Complaints.class);
		complaints.setId("7");
		complaints.setComplaintStatusCode("INPR");
		when(complaintService.getComplaintDetails(complaints.getId())).thenReturn(complaints);
		when(businessObjectService.getBusinessObjectsBasedOnBusinessObjectId(streams.getId())).
				thenReturn(streams);
		when(streamDao.checkAllStreamStatus(any(String.class))).thenReturn(result);
		Streams  streams1=Struct.create(Streams.class);
		streams1.setId("5");
		streams1.setParentIDId("7");
		streams1.setIsRelevant(true);
		Optional<Streams> emptyOpt = Optional.of(streams1);
		when(db.run(any(CqnInsert.class))).thenReturn(result);
		when(result.first(Streams.class)).thenReturn(emptyOpt);
		Row row=Struct.create(Row.class);
		List<Row> rowvalues = new ArrayList<>();
		row.put("code", "100");
		row.put("name", "F001");
		row.put("descr", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		List<Streams> streamsList=new ArrayList<>();
		streamsList.add(streams1);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(Streams.class)).thenReturn(streamsList);
		when(result.list()).thenReturn(rowvalues);

		impl.updateStreamStatus(streams.getId(),streams.getId(),true);
	}

	@Test(expected = Exception.class)
	public void updateStreamStatusExpTest() {
		BusinessObjects streams = Struct.create(BusinessObjects.class);
		streams.setBusinessObjectTypeCode("1");
		streams.setId("8");
		streams.setStreamId("3");
		streams.setComplaint("7");

		impl.updateStreamStatus(streams.getId(),streams.getId(),true);
	}
	@Test
	public void checkAllStreamStatusTest() {
		List<Streams> streamsArrayList = new ArrayList<>();
		Streams streams =  Struct.create(Streams.class);
        Complaints complaints = Struct.create(Complaints.class);
        complaints.setComplaintStatusCode("INPR"); 
		streams.setId("100");
		streams.setParentIDId("F001");
		streams.setStreamTypeCode("QLTY");
		streams.setStatusCode("INPR");
		streamsArrayList.add(streams);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(Streams.class)).thenReturn(streamsArrayList);
		List<Row> rowvalues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		row.put("ID", "100");
		row.put("status_code", "CLSD");
		row.put("materialType", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(result.list()).thenReturn(rowvalues);
		when(streamDao.checkAllStreamStatus(any(String.class))).thenReturn(result);
        when(complaintService.getComplaintDetails(streams.getId())).thenReturn(complaints);
		impl.updateComplaintStatusBasedOnAllStreamStatus(streams.getId());
	}
	
	//public void 

	@Test
	public void checkAllStreamStatusNullTest() {
		List<Streams> streamsArrayList = new ArrayList<>();
		Streams streams =  Struct.create(Streams.class);
		streams.setId("100");
		streams.setParentIDId("F001");
		streams.setStreamTypeCode("test");
		streams.setStatusCode("PT");
		streamsArrayList.add(streams);
		Complaints complaints = Struct.create(Complaints.class);
        complaints.setComplaintStatusCode("INPR"); 
		when(complaintService.getComplaintDetails(streams.getId())).thenReturn(complaints);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.listOf(Streams.class)).thenReturn(streamsArrayList);
		List<Row> rowvalues = new ArrayList<>();
		Row row=Struct.create(Row.class);
		row.put("ID", "100");
		row.put("status_code", "CLSD");
		row.put("materialType", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		when(result.list()).thenReturn(rowvalues);
		when(streamDao.checkAllStreamStatus(any(String.class))).thenReturn(result);
		impl.updateComplaintStatusBasedOnAllStreamStatus(streams.getId());
	}
	
	@Test
	public void testDetermineStreamStatusAndMarkClosed() {
		Streams streams =  Struct.create(Streams.class);
		streams.setId("100");
		streams.setParentIDId("F001");
		streams.setStreamTypeCode("QLTY");
		streams.setStatusCode("INPR");
		streams.setParentIDId("7");
		List<Streams> relevantStreamList = new ArrayList<>();
		relevantStreamList.add(streams);
		BusinessObjects businessObjects = Struct.create(BusinessObjects.class);
		businessObjects.setBusinessObjectTypeCode("QN");
		businessObjects.setId("8");
		businessObjects.setStreamId("100");
		businessObjects.setComplaint("7");
		when(businessObjectService.getBusinessObjectIdBasedOnTypeAndComplaint(streams.getParentIDId(),"QN")).thenReturn(businessObjects.getId());
		when(businessObjectService.getCurrentBOStatusSupplier8D(businessObjects.getId())).thenReturn("QNCMPL");
		when(businessObjectService.getBusinessObjectStatusBasedOnType(streams.getParentIDId(), Constants.SUPPLIER_EIGHTD_CODE)).thenReturn("");
		when(businessObjectService.checkIfBOIsRelevant(streams.getParentIDId(),
                Constants.SUPPLIER_EIGHTD_CODE)).thenReturn(false);
		when(streamDao.findQualityStreamStatus(any(String.class), any(Map.class))).thenReturn(result);
		impl.determineStreamStatusAndMarkClosed(false, relevantStreamList);
	}
	
}
