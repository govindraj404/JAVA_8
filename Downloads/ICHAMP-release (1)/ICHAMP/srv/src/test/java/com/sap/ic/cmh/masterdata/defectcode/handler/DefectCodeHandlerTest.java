package com.sap.ic.cmh.masterdata.defectcode.handler;

import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.cds.CdsCreateEventContext;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.cds.CdsUpdateEventContext;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.masterdata.defectcode.service.DefectCodeService;
import com.sap.ic.cmh.masterdata.defectcode.validation.DefectCodeValidator;
import cds.gen.masterdataservice.DefectCodes;
import cds.gen.masterdataservice.DefectGroups;

public class DefectCodeHandlerTest {
	@InjectMocks
	@Autowired
	private DefectCodesHandler handler;
	@Mock
	private DefectCodeValidator validator;
	@Mock
	Messages messages;
	@Mock
	CdsCreateEventContext createContextMock;
	@Mock
	CqnInsert cqnInsert;
	@Mock
	CqnUpdate cqnUpdate;
	@Mock
	DefectCodeService defectCodeService;
	@Mock
	Result result;
	@Mock
	private CdsUpdateEventContext updateContextMock;
	@Mock
	private CdsService cdsService;

	private DefectCodes codeItem;
	private DefectGroups group;

	public static final Logger logger = LoggerFactory.getLogger(DefectCodeHandlerTest.class);

	@Before
	public void beforeClass() {
		MockitoAnnotations.openMocks(this);

		group = Struct.create(DefectGroups.class);
		group.setCode("QM-E");
		group.setDescription("test");
		codeItem = Struct.create(DefectCodes.class);
		codeItem.setCode("1");
		codeItem.setDescription("test");
		codeItem.setDefectGroupCode(group.getCode());
		when(createContextMock.getCqn()).thenReturn(cqnInsert);
		List<Map<String, Object>> entries = new ArrayList<>();
		when(cqnInsert.entries()).thenReturn(entries);
	}

	 @Test
	 public void testOnDefectCodeCreate() {
	 Optional<DefectCodes> emptyOpt = Optional.empty();
	 when(defectCodeService.fetchDefectCode(codeItem.getCode(),codeItem.getDefectGroupCode())).thenReturn(codeItem);
	 handler.onDefectCodeCreate(createContextMock, codeItem);
	 }

	@Test
	public void TestBeforeCompanyCodeCreate(){
		when(defectCodeService.fetchDefectCode(codeItem.getCode(),codeItem.getDefectGroupCode())).thenReturn(codeItem);
		handler.beforeDefectCodeCreate(createContextMock, codeItem);
	}

	@Test
	public void updateDefectCodeTest() {

		handler.updateDefectCode(updateContextMock, codeItem);
	}

}
