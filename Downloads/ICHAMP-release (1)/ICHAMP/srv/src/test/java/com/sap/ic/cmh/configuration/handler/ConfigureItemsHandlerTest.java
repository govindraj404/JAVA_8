package com.sap.ic.cmh.configuration.handler;

import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Struct;
import com.sap.cds.services.cds.CdsReadEventContext;
import com.sap.cds.services.request.ParameterInfo;
import com.sap.ic.cmh.configuration.service.ConfigureItemService;

import cds.gen.configurationservice.ComplaintCategories;
import cds.gen.configurationservice.ConfigureItems;

public class ConfigureItemsHandlerTest {
	
	
	@InjectMocks
	@Autowired
	ConfigureItemsHandler configureItemsHandler;
	@Mock
	ConfigureItemService configureItemService;
	@Mock
	CdsReadEventContext context;
	@Mock
	ParameterInfo parameterInfo;
	
	ConfigureItems configureItems;
	ComplaintCategories complaintCategories;
	List<ComplaintCategories> list;
	
	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);
		configureItems = Struct.create(ConfigureItems.class);
		configureItems.setCode("SERMAT");
		configureItems.setName("Service Material");
		complaintCategories = Struct.create(ComplaintCategories.class);
		complaintCategories.setCode("CUSCMP");
		complaintCategories.setName("Customer Complaint");
		list = new ArrayList<>();
		list.add(complaintCategories);
		configureItems.setAssociatedApplications(list);
	}
	
	@Test
	public void testBeforeConfigureItemsRead() {
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", "sample test");
		map1.put("$search", " ");
		map1.put("$select", "search");
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		configureItemsHandler.beforeConfigureItemsRead(context);
	}
	@Test
	public void testBeforeConfigureItemsReadFilterCode() {
		List<ConfigureItems> list = new ArrayList<>();
		list.add(configureItems);
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", "code eq 'SER'");
		map1.put("$search", null);
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		when(configureItemService.getAllConfiguredApplications()).thenReturn(list);
		configureItemsHandler.beforeConfigureItemsRead(context);
	}
	@Test
	public void testBeforeConfigureItemsReadFilterAssociatedAppCode() {
		List<ConfigureItems> list = new ArrayList<>();
		list.add(configureItems);
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", "(code eq 'SER' or code eq 'DES') and associatedApplications/any(L1:L1/code eq 'cUS')");
		map1.put("$search", null);
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		when(configureItemService.getAllConfiguredApplications()).thenReturn(list);
		configureItemsHandler.beforeConfigureItemsRead(context);
	}
	
	@Test
	public void testBeforeConfigureItemsReadFilterWIthBothCode() {
		List<ConfigureItems> list = new ArrayList<>();
		list.add(configureItems);
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", "associatedApplications/any(L1:L1/code eq 'cUS')");
		map1.put("$search", null);
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		when(configureItemService.getAllConfiguredApplications()).thenReturn(list);
		configureItemsHandler.beforeConfigureItemsRead(context);
	}
	@Test
	public void testBeforeConfigureItemsReadSearchCode() {
		configureItems.setName(null);
		List<ConfigureItems> list = new ArrayList<>();
		list.add(configureItems);
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", null);
		map1.put("$search", " \"SeR\" ");
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		when(configureItemService.getAllConfiguredApplications()).thenReturn(list);
		configureItemsHandler.beforeConfigureItemsRead(context);
	}
	@Test
	public void testBeforeConfigureItemsReadSearchAssociatedAppCode() {
		configureItems.setCode(null);
		configureItems.setName(null);
		List<ConfigureItems> list = new ArrayList<>();
		list.add(configureItems);
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", null);
		map1.put("$search", " \"CUSC\" ");
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		when(configureItemService.getAllConfiguredApplications()).thenReturn(list);
		configureItemsHandler.beforeConfigureItemsRead(context);
	}
	
	@Test
	public void testBeforeConfigureItemsReadSearchName() {
		configureItems.setCode(null);
		List<ConfigureItems> list = new ArrayList<>();
		list.add(configureItems);
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", null);
		map1.put("$search", " \"Material\" ");
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		when(configureItemService.getAllConfiguredApplications()).thenReturn(list);
		configureItemsHandler.beforeConfigureItemsRead(context);
	}
	
	@Test
	public void testBeforeConfigureItemsReadSearchAppName() {
		List<ConfigureItems> list = new ArrayList<>();
		list.add(configureItems);
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", "code eq 'SER'");
		map1.put("$search", " \"customer\" ");
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		when(configureItemService.getAllConfiguredApplications()).thenReturn(list);
		configureItemsHandler.beforeConfigureItemsRead(context);
	}
	
	@Test
	public void testBeforeConfigureItemsReadNull() {
		List<ConfigureItems> list = new ArrayList<>();
		list.add(configureItems);
		Map<String, String> map1 = new HashMap<>();
		map1.put("$filter", null);
		map1.put("$search", null);
		when(parameterInfo.getQueryParams()).thenReturn(map1);
		when(context.getParameterInfo()).thenReturn(parameterInfo);
		when(configureItemService.getAllConfiguredApplications()).thenReturn(list);
		configureItemsHandler.beforeConfigureItemsRead(context);
	}
	
	

}
