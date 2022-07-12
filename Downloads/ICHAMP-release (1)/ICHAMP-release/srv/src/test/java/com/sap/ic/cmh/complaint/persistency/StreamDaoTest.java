package com.sap.ic.cmh.complaint.persistency;

import cds.gen.com.sap.ic.cmh.complainttypestreamtypemapping.ComplaintTypeStreamTypeMappings;
import cds.gen.com.sap.ic.cmh.qualitystreamstatus.QualityStreamStatuses;
import cds.gen.com.sap.ic.cmh.supplierissueprocessstatus.SupplierIssueProcessStatuses;
import cds.gen.complaintservice.BusinessObjects;
import cds.gen.complaintservice.Streams;
import cds.gen.configurationservice.BusinessObjectTypes;
import cds.gen.qualitynotificationservice.QualityNotificationStatuses;
import cds.gen.returnpurchaseorderservice.LogisticsStreamStatuses;
import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.persistence.PersistenceService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

public class StreamDaoTest {

	@InjectMocks
	@Autowired
	StreamDao dao;

	@Mock
	PersistenceService db;

	@Mock
	Result result;
	private Row row;
	private Optional<Row> opt;

	String currentBOStatus;
	Map<String, Object> qnSupplierIssueProcessStatusMap = new HashMap<>();
	List<QualityNotificationStatuses> statusesList = new ArrayList<>();
	QualityNotificationStatuses qualityNotificationStatuses;
	@Mock
	CdsCreateEventContext createContextMock;
	@Mock
	CqnInsert cqnInsert;

	@Before
	public void setup() {
		MockitoAnnotations.openMocks(this);
		currentBOStatus = "BO";
		qnSupplierIssueProcessStatusMap.put("ID", "id");

		QualityNotificationStatuses status = Struct.create(QualityNotificationStatuses.class);
		status.setCode("code");
		status.setSequenceNumber(1234);
		qualityNotificationStatuses = Struct.create(QualityNotificationStatuses.class);
		qualityNotificationStatuses.setCode("12");
		statusesList.add(status);
		row = Struct.create(Row.class);

		when(createContextMock.getCqn()).thenReturn(cqnInsert);
		List<Map<String, Object>> entries = new ArrayList<>();
		when(cqnInsert.entries()).thenReturn(entries);
	}

	@Test
	public void testFindClaimStreamStatus() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		dao.findClaimStreamStatus("CREATED");
	}

	@Test
	public void findClaimStreamStatusTest() {
		qnSupplierIssueProcessStatusMap.put("S8D", "S8DCMPL");
		qnSupplierIssueProcessStatusMap.put("QN", "QNINPR");
		Optional<QualityNotificationStatuses> emptyOpt = Optional.of(qualityNotificationStatuses);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityNotificationStatuses.class)).thenReturn(emptyOpt);
		dao.findQualityStreamStatus(currentBOStatus, qnSupplierIssueProcessStatusMap);
	}

	@Test
	public void findClaimStreamStatusZeroTest() {
		qnSupplierIssueProcessStatusMap.put("S8D", "S8DCMPL");
		qnSupplierIssueProcessStatusMap.put("QN", "QNCMPL");
		Optional<QualityNotificationStatuses> emptyOpt = Optional.of(qualityNotificationStatuses);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityNotificationStatuses.class)).thenReturn(emptyOpt);
		// when(dao.getQualityNotificationStatusSequenceNumber(any(String.class))).thenReturn("2");
		// when(dao.getAllQualityNotificationStatuses()).thenReturn("2");
		dao.findQualityStreamStatus(currentBOStatus, qnSupplierIssueProcessStatusMap);
	}
	@Test
	public void testFindClaimStreamStatusZero() {
		qnSupplierIssueProcessStatusMap.put("QN","2");
		qnSupplierIssueProcessStatusMap.put("isRelevant",false);
		qnSupplierIssueProcessStatusMap.put("S8D", "S8DCMPL");
		Optional<QualityNotificationStatuses> emptyOpt = Optional.of(qualityNotificationStatuses);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityNotificationStatuses.class)).thenReturn(emptyOpt);
		List<Row> rowvalues = new ArrayList<>();
		row.put("sequenceNumber", "0");
		row.put("ID", "0");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		dao.findQualityStreamStatus(currentBOStatus, qnSupplierIssueProcessStatusMap);
	}

	@Test
	public void testFindClaimStreamStatusTwo() {
		qnSupplierIssueProcessStatusMap.put("QN","2");
		qnSupplierIssueProcessStatusMap.put("S8D","S8DINPR");
		qnSupplierIssueProcessStatusMap.put("isRelevant",false);
		Optional<QualityNotificationStatuses> emptyOpt = Optional.of(qualityNotificationStatuses);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityNotificationStatuses.class)).thenReturn(emptyOpt);
		List<Row> rowvalues = new ArrayList<>();
		row.put("sequenceNumber", "0");
		row.put("QN","2");

		opt = Optional.of(row);
		rowvalues.add(row);
		Row row1 = Struct.create(Row.class);
		List<Row> rowvalues1 = new ArrayList<>();
		row1.put("sequenceNumber", "1");
		rowvalues1.add(row1);
		when(result.list()).thenReturn(rowvalues).thenReturn(rowvalues1);
		when(result.first()).thenReturn(opt);
		dao.findQualityStreamStatus(currentBOStatus, qnSupplierIssueProcessStatusMap);
	}

	@Test
	public void testFindClaimStreamStatusOne() {
		qnSupplierIssueProcessStatusMap.put("QN", "QNINPR");
		qnSupplierIssueProcessStatusMap.put("S8D", "S8DINPR");
		Optional<QualityNotificationStatuses> emptyOpt = Optional.of(qualityNotificationStatuses);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityNotificationStatuses.class)).thenReturn(emptyOpt);
		List<Row> rowvalues = new ArrayList<>();
		row.put("sequenceNumber", "0");
		row.put("QN","1");

		opt = Optional.of(row);
		rowvalues.add(row);
		Row row1 = Struct.create(Row.class);
		List<Row> rowvalues1 = new ArrayList<>();
		row1.put("sequenceNumber", "1");
		rowvalues1.add(row1);
		when(result.list()).thenReturn(rowvalues).thenReturn(rowvalues1);
		when(result.first()).thenReturn(opt);
		dao.findQualityStreamStatus(currentBOStatus, qnSupplierIssueProcessStatusMap);
	}

	@Test
	public void getAllQualityNotificationStatusesTest() {
		Optional<QualityNotificationStatuses> emptyOpt = Optional.of(qualityNotificationStatuses);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityNotificationStatuses.class)).thenReturn(emptyOpt);
		dao.getAllQualityNotificationStatuses();
	}

	@Test
	public void getQualityNotificationStatusSequenceNumberTest() {
		qualityNotificationStatuses.setCode("2");
		Optional<QualityNotificationStatuses> emptyOpt = Optional.of(qualityNotificationStatuses);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityNotificationStatuses.class)).thenReturn(emptyOpt);
		dao.getQualityNotificationStatusSequenceNumber(qualityNotificationStatuses.getCode());
	}

	@Test
	public void getSupplier8DSequenceTest() {
		SupplierIssueProcessStatuses status = Struct.create(SupplierIssueProcessStatuses.class);
		status.setCode("1");
		Optional<SupplierIssueProcessStatuses> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(SupplierIssueProcessStatuses.class)).thenReturn(emptyOpt);
		dao.getSupplier8DStatusSequence(qualityNotificationStatuses.getCode());
	}

	@Test
	public void getAllSupplier8DStatusesRowCountTest() {
		SupplierIssueProcessStatuses status = Struct.create(SupplierIssueProcessStatuses.class);
		status.setCode("1");
		Optional<SupplierIssueProcessStatuses> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(SupplierIssueProcessStatuses.class)).thenReturn(emptyOpt);
		dao.getAllSupplier8DStatusesRowCount();
	}

	@Test
	public void getQualityStreamStatusTest() {
		QualityStreamStatuses status = Struct.create(QualityStreamStatuses.class);
		status.setStreamStatusCode("1");
		status.setSequenceNumber(10);
		Optional<QualityStreamStatuses> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityStreamStatuses.class)).thenReturn(emptyOpt);
		dao.getQualityStreamStatus(status.getSequenceNumber(),true);
	}
	
	@Test
	public void getQualityStreamStatusSeqNumberTest() {
		QualityStreamStatuses status = Struct.create(QualityStreamStatuses.class);
		status.setStreamStatusCode("1");
		status.setSequenceNumber(2);
		Optional<QualityStreamStatuses> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityStreamStatuses.class)).thenReturn(emptyOpt);
		dao.getQualityStreamStatus(status.getSequenceNumber(),false);
	}

	@Test
	public void getQualityStreamStatusQNEightDTest() {
		qnSupplierIssueProcessStatusMap.put("S8D", "S8DCMPL");
		qnSupplierIssueProcessStatusMap.put("QN", "QNCMPL");
		QualityStreamStatuses status = Struct.create(QualityStreamStatuses.class);
		status.setStreamStatusCode("1");
		Optional<QualityStreamStatuses> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityStreamStatuses.class)).thenReturn(emptyOpt);
		dao.getQualityStreamStatusQNEightD(qnSupplierIssueProcessStatusMap);
	}

	@Test
	public void getQualityStreamStatusQNOrEightD() {
		qnSupplierIssueProcessStatusMap.put("S8D", "S8DCMPL");
		qnSupplierIssueProcessStatusMap.put("QN", "QNCMPL");
		QualityStreamStatuses status = Struct.create(QualityStreamStatuses.class);
		status.setStreamStatusCode("1");
		Optional<QualityStreamStatuses> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityStreamStatuses.class)).thenReturn(emptyOpt);
		dao.getQualityStreamStatusQNOrEightD(qnSupplierIssueProcessStatusMap);
	}

	@Test
	public void findLogisticsStreamStatusTest() {
		LogisticsStreamStatuses status = Struct.create(LogisticsStreamStatuses.class);
		status.setStreamStatusCode("1");
		Optional<LogisticsStreamStatuses> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(LogisticsStreamStatuses.class)).thenReturn(emptyOpt);
		dao.findLogisticsStreamStatus(status.getStreamStatusCode());
	}

	@Test
	public void updateStreamsTest() {
		Streams status = Struct.create(Streams.class);
		status.setId("1");
		Optional<Streams> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(Streams.class)).thenReturn(emptyOpt);
		dao.updateStreams(status.getId(), status);
	}

	@Test
	public void createStreamTest() {
		Streams status = Struct.create(Streams.class);
		status.setId("1");
		Optional<Streams> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(Streams.class)).thenReturn(emptyOpt);
		dao.createStream(status);
	}

	@Test
	public void getStreamTypeTest() {
		Streams status = Struct.create(Streams.class);
		status.setId("1");
		Optional<Streams> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(Streams.class)).thenReturn(emptyOpt);
		dao.getStreamType();
	}

	@Test
	public void getBusinessObjectTypesTest() {
		BusinessObjectTypes status = Struct.create(BusinessObjectTypes.class);
		status.setCode("1");
		Optional<BusinessObjectTypes> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjectTypes.class)).thenReturn(emptyOpt);
		dao.getBusinessObjectTypes();
	}

	@Test
	public void insertBusinessObjectMappingsTest() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		dao.getBusinessObjectsWithStatus("1", "CLM", "status1");
	}

	@Test
	public void insertBusinessObjectsTest() {
		BusinessObjects status = Struct.create(BusinessObjects.class);
		status.setBusinessObjectIDId("1");
		status.setId("8");
		Optional<BusinessObjects> emptyOpt = Optional.of(status);
		when(db.run(any(CqnInsert.class))).thenReturn(result);
		when(result.first(BusinessObjects.class)).thenReturn(emptyOpt);
		dao.insertBusinessObjects(status);
	}

	@Test
	public void getBusinessObjectTest() {
		BusinessObjects status = Struct.create(BusinessObjects.class);
		status.setBusinessObjectIDId("1");
		status.setId("8");
		Optional<BusinessObjects> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(BusinessObjects.class)).thenReturn(emptyOpt);
		dao.getBusinessObject("4");
	}

	@Test
	public void getStreaTest() {
		Streams status = Struct.create(Streams.class);
		status.setStatusCode("1");
		status.setId("8");
		Optional<Streams> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(Streams.class)).thenReturn(emptyOpt);
		dao.getStream("4");
	}

	@Test
	public void checkAllStreamStatusTest() {
		Streams status = Struct.create(Streams.class);
		status.setStatusCode("1");
		status.setId("8");
		Optional<Streams> emptyOpt = Optional.of(status);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(Streams.class)).thenReturn(emptyOpt);
		dao.checkAllStreamStatus("4");
	}

	@Test
	public void findClaimStreamStatusElseTest() {
		qnSupplierIssueProcessStatusMap.put("S8D","1");
		qnSupplierIssueProcessStatusMap.put("QN","QNCRTD");
		Optional<QualityNotificationStatuses> emptyOpt = Optional.of(qualityNotificationStatuses);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityNotificationStatuses.class)).thenReturn(emptyOpt);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "100");
		row.put("sequenceNumber", "1");
		row.put("plantName", "test");
		row.put("S8D", "2");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);

		dao.findQualityStreamStatus(currentBOStatus, qnSupplierIssueProcessStatusMap);
	}

	@Test
	public void findClaimStreamStatusElse1Test() {
		qnSupplierIssueProcessStatusMap.put("ID","2");
		qnSupplierIssueProcessStatusMap.put("S8D","1");
		qnSupplierIssueProcessStatusMap.put("QN","QNCMPL");
		qnSupplierIssueProcessStatusMap.put("isRelevant",true);
		Optional<QualityNotificationStatuses> emptyOpt = Optional.of(qualityNotificationStatuses);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityNotificationStatuses.class)).thenReturn(emptyOpt);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "100");
		row.put("sequenceNumber", "2");
		row.put("plantName", "test");
		row.put("isRelevant",true);
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		dao.findQualityStreamStatus(currentBOStatus, qnSupplierIssueProcessStatusMap);
	}
	@Test
	public void findClaimStreamStatusElse2Test() {
		qnSupplierIssueProcessStatusMap.put("ID","2");
		qnSupplierIssueProcessStatusMap.put("S8D","0");
		qnSupplierIssueProcessStatusMap.put("QN","QNCMPL");
		qnSupplierIssueProcessStatusMap.put("isRelevant",true);
		Optional<QualityNotificationStatuses> emptyOpt = Optional.of(qualityNotificationStatuses);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityNotificationStatuses.class)).thenReturn(emptyOpt);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "100");
		row.put("sequenceNumber", "3");
		row.put("plantName", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		dao.findQualityStreamStatus(currentBOStatus, qnSupplierIssueProcessStatusMap);
	}
	@Test
	public void findClaimStreamStatusElse3Test() {
		qnSupplierIssueProcessStatusMap.put("ID","2");
		qnSupplierIssueProcessStatusMap.put("S8D","5");
		qnSupplierIssueProcessStatusMap.put("QN","QNINPR");
		qnSupplierIssueProcessStatusMap.put("isRelevant",true);
		Optional<QualityNotificationStatuses> emptyOpt = Optional.of(qualityNotificationStatuses);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(QualityNotificationStatuses.class)).thenReturn(emptyOpt);
		List<Row> rowvalues = new ArrayList<>();
		row.put("ID", "100");
		row.put("sequenceNumber", "1");
		row.put("plantName", "test");
		Optional<Row> opt = Optional.of(row);
		rowvalues.add(row);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		dao.findQualityStreamStatus(currentBOStatus, qnSupplierIssueProcessStatusMap);
	}

	@Test
	public void testGetSequenceBasedOnComplaintStreamType() {
		ComplaintTypeStreamTypeMappings complaintTypeStreamTypeMappings = Struct.create(ComplaintTypeStreamTypeMappings.class);
		complaintTypeStreamTypeMappings.setStreamTypeCode("1234");
		Optional<ComplaintTypeStreamTypeMappings> emptyOpt = Optional.of(complaintTypeStreamTypeMappings);
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		when(result.first(ComplaintTypeStreamTypeMappings.class)).thenReturn(emptyOpt);
		dao.getSequenceBasedOnComplaintStreamType("1234", "26167");
	}

	@Test
	public void testFindMaximumSequenceNumber() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("sequenceNumber", "2");
		opt = Optional.of(row);
		rowvalues.add(row);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		dao.findMaximumSequenceNumber(1,1);
	}

	@Test
	public void testFindMaximumSequenceNumber1() {
		when(db.run(any(CqnSelect.class))).thenReturn(result);
		List<Row> rowvalues = new ArrayList<>();
		row.put("sequenceNumber", "2");
		opt = Optional.of(row);
		rowvalues.add(row);
		row.put("ID", "2");
		rowvalues.add(row);
		row.put("G_ID", "2");
		rowvalues.add(row);
		when(result.list()).thenReturn(rowvalues);
		when(result.first()).thenReturn(opt);
		dao.findMaximumSequenceNumber(3,1);
	}
}