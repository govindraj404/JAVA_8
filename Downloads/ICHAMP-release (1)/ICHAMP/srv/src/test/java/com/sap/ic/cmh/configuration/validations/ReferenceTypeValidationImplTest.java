package com.sap.ic.cmh.configuration.validations;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.services.messages.Message;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.configuration.persistency.ReferenceTypeDao;
import com.sap.ic.cmh.customercomplaint.referencedocumentcategory.persistency.ReferenceDocumentCategoriesDao;
import com.sap.ic.cmh.utils.datavalidation.DataValidator;
import com.sap.cds.Row;
import java.util.Optional;
import cds.gen.configurationservice.ReferenceTypes;

public class ReferenceTypeValidationImplTest {

	@InjectMocks
	ReferenceTypeValidationImpl referenceTypeValidationImpl;
	
	@Mock
	Messages messages;

	@Mock
	private Message msg;
	
	@Mock
	DataValidator dataValidator;
	
	@Mock
	ReferenceTypeDao referenceTypeDao;
	
	@Mock
	ReferenceDocumentCategoriesDao referenceDocumentCategoriesDao;
	
	@Mock	
	Result result;
	
	ReferenceTypes referenceTypes;
	private Row row;
	private Optional<Row> opt;
	
	@Before
	public void setBefore() {
		MockitoAnnotations.openMocks(this);
		referenceTypes=Struct.create(ReferenceTypes.class);
		 row = Struct.create(Row.class);
		 row.put("code", "CODE100");
		 row.put("ID", "11");
		 opt = Optional.of(row);	
	}
	
	@Test
	public void testValidateReferenceType() {
		when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		when(msg.target(any(String.class))).thenReturn(msg);
		referenceTypeValidationImpl.validateReferenceType(referenceTypes);
	}
	
	@Test
	public void testValidateCode() {
		referenceTypes.setCode("code");
		referenceTypes.setId("ID2");
		List<Row> rowList=new ArrayList<>();
		rowList.add(row);
		when(referenceTypeDao.getReferenceTypeBasedOnCode("code")).thenReturn(result);
		when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		when(msg.target(any(String.class))).thenReturn(msg);
		when(result.list()).thenReturn(rowList);
		when(result.first()).thenReturn(opt);
		referenceTypeValidationImpl.validateCode(referenceTypes);
	}
	
	@Test
	public void testValidateReferenceDocumentCategory() {
		ReferenceTypes referenceTypes1=Struct.create(ReferenceTypes.class);
		referenceTypes1.setReferenceDocumentCategoryCode("code");
		when(referenceDocumentCategoriesDao.getReferenceDocumentCategoriesBasedOnCode("code")).thenReturn(result);
		when(result.first()).thenReturn(opt);
		referenceTypeValidationImpl.validateReferenceDocumentCategory(referenceTypes1);
	}
	
	@Test
	public void testValidateReferenceDocumentCategoryWithNullData() {
		opt=Optional.empty();
		ReferenceTypes referenceTypes1=Struct.create(ReferenceTypes.class);
		referenceTypes1.setReferenceDocumentCategoryCode("code");
		when(referenceDocumentCategoriesDao.getReferenceDocumentCategoriesBasedOnCode("code")).thenReturn(result);
		when(result.first()).thenReturn(opt);
		when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		when(msg.target(any(String.class))).thenReturn(msg);
		referenceTypeValidationImpl.validateReferenceDocumentCategory(referenceTypes1);
	}
	@Test
	public void testValidateCodeWithEmptyList() {
		referenceTypes.setCode("code");
		referenceTypes.setId("ID2");
		List<Row> rowList=new ArrayList<>();
		when(referenceTypeDao.getReferenceTypeBasedOnCode("code")).thenReturn(result);
		when(result.list()).thenReturn(rowList);
		referenceTypeValidationImpl.validateCode(referenceTypes);
	}
	@Test
	public void testValidateCodeWithSameID() {
		referenceTypes.setCode("code");
		referenceTypes.setId("11");
		List<Row> rowList=new ArrayList<>();
		rowList.add(row);
		when(referenceTypeDao.getReferenceTypeBasedOnCode("code")).thenReturn(result);
		when(messages.error(any(String.class), any(Object[].class))).thenReturn(msg);
		when(msg.target(any(String.class))).thenReturn(msg);
		when(result.list()).thenReturn(rowList);
		when(result.first()).thenReturn(opt);
		referenceTypeValidationImpl.validateCode(referenceTypes);
	}
}


