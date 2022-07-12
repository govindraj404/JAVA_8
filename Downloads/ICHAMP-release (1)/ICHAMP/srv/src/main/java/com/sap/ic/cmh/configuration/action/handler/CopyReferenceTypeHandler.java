package com.sap.ic.cmh.configuration.action.handler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.sap.cds.Struct;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;

import cds.gen.configurationservice.CopyReferenceTypesContext;
import cds.gen.configurationservice.ReferenceTypes;
import cds.gen.configurationservice.ReferenceTypes_;

@Component
@ServiceName("ConfigurationService")
public class CopyReferenceTypeHandler implements EventHandler {
	
	@Qualifier("ConfigurationService")
	@Autowired
	DraftService draftService;
	
	@Autowired
	Messages messages;
	
	private static final Logger logger = LoggerFactory.getLogger(CopyReferenceTypeHandler.class);
	private static final String COPY_REFERENCE_TYPE_HANDLER = "CopyReferenceTypeHandler";
	
	/**
	 * Perform copy action on reference type
	 * 
	 * @param {@link CopyReferenceTypesContext} context
	 * 
	 * @public
	 */
	@On(entity = ReferenceTypes_.CDS_NAME)
	public void copyReferenceType(final CopyReferenceTypesContext context) {
		LoggerHelper.logMethodEntry(logger, COPY_REFERENCE_TYPE_HANDLER, "copyReferenceType");
		CqnSelect select = context.getCqn();
		CdsService service = (CdsService)context.getService();
		ReferenceTypes referenceType = (service).run(select).single(ReferenceTypes.class);
		ReferenceTypes copyReferenceType= Struct.create(ReferenceTypes.class);
		copyReferenceType.setCode(referenceType.getCode());
		copyReferenceType.setDescription(referenceType.getDescription());
		copyReferenceType.setReferenceDocumentCategoryCode(referenceType.getReferenceDocumentCategoryCode());
		
		context.setResult(draftService.newDraft(Insert.into(ReferenceTypes_.class).
				entry(copyReferenceType)).single(ReferenceTypes.class));
		messages.success(MessageKeys.COPY_REFERENCE_TYPE_ACTION_SUCCESSFULLY_PERFORMED);
		context.setCompleted();
		LoggerHelper.logMethodExit(logger, COPY_REFERENCE_TYPE_HANDLER, "copyReferenceType");
	}
}



