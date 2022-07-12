package com.sap.ic.cmh.customercomplaint.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.ic.cmh.customercomplaint.persistency.AttachmentsDao;
import com.sap.ic.cmh.utils.PlatformUtil;

import cds.gen.customercomplaintservice.Attachments;

public class AttachmentOperationsServiceImplTest {
	
	@InjectMocks
    @Autowired
	AttachmentOperationsServiceImpl attachmentOperationsServiceImpl;
	@Mock
	PlatformUtil platformUtil;
	@Mock
	AttachmentsDao attachmentsDao;
	@Mock
    Result result;
	@Mock
	RestTemplate restTemplate;
	 private Row row;
	 private Optional<Row> opt;
	Attachments attachments;
	
	@Before
	public void beforeClass() {
	  MockitoAnnotations.openMocks(this);
	  attachments = Struct.create(Attachments.class);
	  attachments.setId("1");
	  attachments.setParentID("1234");
	  attachments.setName("Test.png");
	  attachments.setType("image/png");
	  attachments.setSize("1024");
	  
	  row = Struct.create(Row.class);
	}
	
	@Test
	public void testStoreAttachmentInHanaDb() {
		attachmentOperationsServiceImpl.storeAttachmentInHanaDb(attachments.getName(), "1111", attachments.getSize(),
				attachments.getType(), "43");;
	}
	
	@Test(expected = Exception.class)
	public void testCheckForMalware() {
		byte[] byteArray = new String("test").getBytes();
		JSONObject jsonObj=new JSONObject("{\"uri\":\"test.com\",\"password\":\"password\",\"username\":\"username\"}");
	
		String vcapservice_env = System.getenv("VCAP_SERVICES");
		Mockito.when(platformUtil.getEnvironmentDetail()).thenReturn(vcapservice_env);
		Mockito.when(platformUtil.getCredentials("malware-scanner")).thenReturn(jsonObj);

		String body = "{\"malwareDetected\":\"true\",\"status_message\":\"SUCCEEDED\"}";
		HttpHeaders headers = new HttpHeaders();
		headers.add("Content-Type","text/plain");
		ResponseEntity<String> value = new ResponseEntity<String>(body,headers,HttpStatus.OK);
		HttpEntity<byte[]> entity = new HttpEntity<>(byteArray, headers);
		Mockito.when(restTemplate.exchange(any(String.class), HttpMethod.POST, entity,String.class)).thenReturn(value);
		
		attachmentOperationsServiceImpl.checkForMalware(byteArray); 
	}
	@Test
	public void testCheckForMalwareNull() {
		String vcapservice_env = System.getenv("VCAP_SERVICES");
		Mockito.when(platformUtil.getEnvironmentDetail()).thenReturn(vcapservice_env);
		Mockito.when(platformUtil.getCredentials("malware-scanner")).thenReturn(null);
		byte[] byteArray = new String("test").getBytes();
		attachmentOperationsServiceImpl.checkForMalware(byteArray); 
	}
	@Test
	public void testDeleteAttachmentInHanaDb() {
		attachmentOperationsServiceImpl.deleteAttachmentInHanaDb(attachments.getId());
	}
	@Test
	public void testGetAttachmentDetails() {
		List<Attachments> list = new ArrayList<>();
		list.add(attachments);
		List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.listOf(Attachments.class)).thenReturn(list);
		when(attachmentsDao.getAttachmentDetailsBasedOnId(attachments.getId())).thenReturn(result);
		attachmentOperationsServiceImpl.getAttachmentDetails(attachments.getId());
	}
	
	@Test
	public void testGetAttachmentDetailsNull() {
		List<Attachments> list = new ArrayList<>();
		list.add(attachments);
		List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ID");
        opt = Optional.empty();
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.listOf(Attachments.class)).thenReturn(list);
		when(attachmentsDao.getAttachmentDetailsBasedOnId(attachments.getId())).thenReturn(result);
		attachmentOperationsServiceImpl.getAttachmentDetails(attachments.getId());
	}
	
	@Test
	public void testRenameAttachments() {
		Map<String, Object> data = Collections.singletonMap(Attachments.NAME, "updated");
		attachmentOperationsServiceImpl.renameAttachment("updated", attachments.getId());
	}
	
	@Test
	public void testGetDraftAttachmentDetailsBasedOnComplaintId() {
		List<Attachments> list = new ArrayList<>();
		list.add(attachments);
		List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.listOf(Attachments.class)).thenReturn(list);
		when(attachmentsDao.getDraftAttachmentDetailsBasedOnComplaintId(attachments.getParentID())).thenReturn(result);
		attachmentOperationsServiceImpl.getDraftAttachmentDetailsBasedOnComplaintId(attachments.getParentID());
	}
	
	@Test
	public void testGetDraftAttachmentDetailsBasedOnComplaintIdNull() {
		List<Attachments> list = new ArrayList<>();
		list.add(attachments);
		List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ID");
        opt = Optional.empty();
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.listOf(Attachments.class)).thenReturn(list);
		when(attachmentsDao.getDraftAttachmentDetailsBasedOnComplaintId(attachments.getParentID())).thenReturn(result);
		attachmentOperationsServiceImpl.getDraftAttachmentDetailsBasedOnComplaintId(attachments.getParentID());
	}
	
	@Test
	public void testGetAttachmentDetailsBasedOnComplaintId() {
		List<Attachments> list = new ArrayList<>();
		list.add(attachments);
		List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.listOf(Attachments.class)).thenReturn(list);
		when(attachmentsDao.getAttachmentDetailsBasedOnComplaintId(attachments.getParentID())).thenReturn(result);
		attachmentOperationsServiceImpl.getAttachmentDetailsBasedOnComplaintId(attachments.getParentID());
	}
	
	@Test
	public void testGetAttachmentDetailsBasedOnComplaintIdNull() {
		List<Attachments> list = new ArrayList<>();
		list.add(attachments);
		List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ID");
        opt = Optional.empty();
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.listOf(Attachments.class)).thenReturn(list);
		when(attachmentsDao.getAttachmentDetailsBasedOnComplaintId(attachments.getParentID())).thenReturn(result);
		attachmentOperationsServiceImpl.getAttachmentDetailsBasedOnComplaintId(attachments.getParentID());
	}
	
	@Test
	public void testGetDraftAttachmentDetailsHasActiveEntity() {
		List<Attachments> list = new ArrayList<>();
		list.add(attachments);
		List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ID");
        opt = Optional.of(row);
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.listOf(Attachments.class)).thenReturn(list);
		when(attachmentsDao.getDraftAttachmentDetailsHasActiveEntity(attachments.getParentID())).thenReturn(result);
		attachmentOperationsServiceImpl.getDraftAttachmentDetailsHasActiveEntity(attachments.getParentID());
	}
	
	@Test
	public void testGetDraftAttachmentDetailsHasActiveEntityNull() {
		List<Attachments> list = new ArrayList<>();
		list.add(attachments);
		List<Row> rowvalues = new ArrayList<>();
        row.put("ID", "ID");
        opt = Optional.empty();
        rowvalues.add(row);
        when(result.list()).thenReturn(rowvalues);
        when(result.first()).thenReturn(opt);
        when(result.listOf(Attachments.class)).thenReturn(list);
		when(attachmentsDao.getDraftAttachmentDetailsHasActiveEntity(attachments.getParentID())).thenReturn(result);
		attachmentOperationsServiceImpl.getDraftAttachmentDetailsHasActiveEntity(attachments.getParentID());
	}
	
}
