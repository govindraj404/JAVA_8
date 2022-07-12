package com.sap.ic.cmh.configuration.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Optional;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;

import com.sap.cds.Result;
import com.sap.cds.Row;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.runtime.CdsRuntime;
import com.sap.ic.cmh.configuration.persistency.ConfigureItemDao;
import com.sap.ic.cmh.utils.LocaleMessageHelper;

import cds.gen.configurationservice.ComplaintCategories;
import cds.gen.configurationservice.ConfigureItems;

public class ConfigureItemServiceTest {
	
	@InjectMocks
	@Autowired
	ConfigureItemServiceImpl configureItemServiceImpl;
	
	@Mock	
	ConfigureItemDao configureItemDao;
	
	@Mock
	Result result;
	@Mock
	Result result1;
	@Mock
	LocaleMessageHelper localeMessageHelper;
	@Mock
	Messages messages;
	@Mock
	Message msg;
	private Row row;
	private Row row1;
	private Optional<Row> opt;
	private Optional<Row> opt1;
	
	ConfigureItems configureItems;
	ConfigureItems configureItems1;
	ComplaintCategories complaintCategories;
	ComplaintCategories complaintCategories1;
	List<ComplaintCategories> list;
	List<ComplaintCategories> list1;
	
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		configureItems = Struct.create(ConfigureItems.class);
		configureItems.setCode("CONTYPE");
		configureItems.setName("test");
		complaintCategories = Struct.create(ComplaintCategories.class);
		complaintCategories.setCode("CUSCMP");
		complaintCategories.setName("Customer Complaint");
		list = new ArrayList<>();
		list.add(complaintCategories);
		configureItems.setAssociatedApplications(list);
		
		configureItems1 = Struct.create(ConfigureItems.class);
		configureItems1.setCode("SERMAT");
		configureItems1.setName("test");
		complaintCategories1 = Struct.create(ComplaintCategories.class);
		complaintCategories1.setCode("SREC");
		complaintCategories1.setName("Supplier Recovery");
		list1 = new ArrayList<>();
		list1.add(complaintCategories);
		configureItems1.setAssociatedApplications(list1);
		
		row = Struct.create(Row.class);
		row1 = Struct.create(Row.class);
	}
	
	@Test
	public void testGetAllConfiguredApplications() {
		List<ComplaintCategories> configureItemsList = new ArrayList<>();
		
		configureItemsList.add(complaintCategories);
		row.put("code", "CUSCMP");
		row.put("name", "Customer Complaint");
		opt = Optional.of(row);
		when(result.listOf(ComplaintCategories.class)).thenReturn(configureItemsList);
		when(result.first()).thenReturn(opt);
		when(configureItemDao.getComplaintCategoriesBasedOnCode("CUSCMP")).thenReturn(result);
		
		List<ComplaintCategories> configureItemsList1 = new ArrayList<>();
		configureItemsList1.add(complaintCategories1);
		row1.put("code", "SREC");
		row1.put("name", "Supplier Recovery");
		opt1 = Optional.of(row1);
		when(result1.listOf(ComplaintCategories.class)).thenReturn(configureItemsList1);
		when(result1.first()).thenReturn(opt1);
		when(configureItemDao.getComplaintCategoriesBasedOnCode("SREC")).thenReturn(result1);
		Mockito.when(localeMessageHelper.getMessage(Mockito.anyString())).thenReturn("");
		configureItemServiceImpl.getAllConfiguredApplications();
	}
	
	@Test
	public void testGetAllConfiguredApplicationsNull() {
		List<ComplaintCategories> configureItemsList = new ArrayList<>();
		
		configureItemsList.add(complaintCategories);
		row.put("code", "");
		row.put("name", "");
		opt = Optional.empty();
		when(result.listOf(ComplaintCategories.class)).thenReturn(configureItemsList);
		when(result.first()).thenReturn(opt);
		when(configureItemDao.getComplaintCategoriesBasedOnCode("CUSCMP")).thenReturn(result);
		
		List<ComplaintCategories> configureItemsList1 = new ArrayList<>();
		configureItemsList1.add(complaintCategories1);
		row1.put("code", "");
		row1.put("name", "");
		opt1 = Optional.empty();
		when(result1.listOf(ComplaintCategories.class)).thenReturn(configureItemsList1);
		when(result1.first()).thenReturn(opt1);
		when(configureItemDao.getComplaintCategoriesBasedOnCode("SREC")).thenReturn(result1);
		Mockito.when(localeMessageHelper.getMessage(Mockito.anyString())).thenReturn("");
		configureItemServiceImpl.getAllConfiguredApplications();
	}
	
	@Test
	public void testGetComplaintCategoriesBasedOnCode() {
		List<ComplaintCategories> configureItemsList = new ArrayList<>();
		configureItemsList.add(complaintCategories);
		row.put("code", "CUSCMP");
		row.put("name", "Customer Complaint");
		opt = Optional.of(row);
		when(result.listOf(ComplaintCategories.class)).thenReturn(configureItemsList);
		when(result.first()).thenReturn(opt);
		when(configureItemDao.getComplaintCategoriesBasedOnCode("CUSCMP")).thenReturn(result);
		configureItemServiceImpl.getComplaintCategoriesBasedOnCode("CUSCMP");
	}
	
	@Test
	public void testGetComplaintCategoriesBasedOnCodeNull() {
		List<ComplaintCategories> configureItemsList = new ArrayList<>();
		configureItemsList.add(complaintCategories);
		row.put("code", "CUSCMP");
		row.put("name", "Customer Complaint");
		opt = Optional.empty();
		when(result.listOf(ComplaintCategories.class)).thenReturn(configureItemsList);
		when(result.first()).thenReturn(opt);
		when(configureItemDao.getComplaintCategoriesBasedOnCode("CUSCMP")).thenReturn(result);
		configureItemServiceImpl.getComplaintCategoriesBasedOnCode("CUSCMP");
	}

}
