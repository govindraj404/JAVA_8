package com.sap.ic.cmh.configuration.action.handler;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.sap.cds.Result;
import com.sap.cds.Struct;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.messages.Messages;

import cds.gen.configurationservice.CopyReferenceTypesContext;
import cds.gen.configurationservice.ReferenceTypes;
import com.sap.cds.ql.cqn.CqnInsert;

public class CopyReferenceTypeHandlerTest {
	
	@InjectMocks
	CopyReferenceTypeHandler handler;
	
	@Mock
	DraftService draftService;
	
	@Mock
	Messages messages;
	
	@Mock
	CopyReferenceTypesContext context;
	
	 @Mock
	 CdsService cdsService;

	 @Mock
	 CqnSelect cqnSelect;
	 
	 @Mock
	 Result result;
	 
	 ReferenceTypes referenceTypes;
	 
	 @Before
	 public void setBefore() {
		 MockitoAnnotations.openMocks(this);
		 referenceTypes=Struct.create(ReferenceTypes.class);
	 }
	
	 @Test
	 public void testCopyReferenceType() {
	 when(context.getCqn()).thenReturn(cqnSelect);
	 when(context.getService()).thenReturn(cdsService);
	 when(cdsService.run(any(CqnSelect.class))).thenReturn(result);
	 when(result.single(ReferenceTypes.class)).thenReturn(referenceTypes);
	 when(draftService.newDraft(any(CqnInsert.class))).thenReturn(result);
	 handler.copyReferenceType(context);
	}
	

}
