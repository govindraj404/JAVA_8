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

import cds.gen.configurationservice.CopyItemCategoriesContext;
import cds.gen.configurationservice.ItemCategories;
import cds.gen.configurationservice.ItemCategories_;

@Component
@ServiceName("ConfigurationService")
public class CopyItemCategoryHandler implements EventHandler {
	
	@Qualifier("ConfigurationService")
	@Autowired
	DraftService draftService;
	
	@Autowired
	Messages messages;
	
	private static final Logger logger = LoggerFactory.getLogger(CopyItemCategoryHandler.class);
	private static final String COPY_ITEM_CATEGORY_HANDLER = "CopyItemCategoryHandler";
	
	/**
	 * Perform copy action on item category
	 * 
	 * @param {@link CopyItemCategoriesContext} context
	 * 
	 * @public
	 */
	@On(entity = ItemCategories_.CDS_NAME)
	public void copyItemCategory(final CopyItemCategoriesContext context) {
		LoggerHelper.logMethodEntry(logger, COPY_ITEM_CATEGORY_HANDLER, "copyItemCategory");
		CqnSelect select = context.getCqn();
		CdsService service = (CdsService)context.getService();
		ItemCategories itemCategory = (service).run(select).single(ItemCategories.class);
		ItemCategories copyItemCategory = Struct.create(ItemCategories.class);
		copyItemCategory.setCode(itemCategory.getCode());
		copyItemCategory.setComplaintCategoryCode(itemCategory.getComplaintCategoryCode());
		copyItemCategory.setComplaintQuantityRuleCode(itemCategory.getComplaintQuantityRuleCode());
		copyItemCategory.setDescription(itemCategory.getDescription());
		copyItemCategory.setExternalReferenceCheckedForDuplication(itemCategory.getExternalReferenceCheckedForDuplication());
		copyItemCategory.setExternalReferenceMandatory(itemCategory.getExternalReferenceMandatory());
		copyItemCategory.setIndividualComplaint(itemCategory.getIndividualComplaint());
		copyItemCategory.setMaterialEnteredManually(itemCategory.getMaterialEnteredManually());
		copyItemCategory.setMaterialId(itemCategory.getMaterialId());
		copyItemCategory.setReceivedQuantityEditable(itemCategory.getReceivedQuantityEditable());
		copyItemCategory.setReferenceDocumentCategoryCode(itemCategory.getReferenceDocumentCategoryCode());
		copyItemCategory.setReturnQuantityEditable(itemCategory.getReturnQuantityEditable());
		
		context.setResult(draftService.newDraft(Insert.into(ItemCategories_.class).
				entry(copyItemCategory)).single(ItemCategories.class));
		messages.success(MessageKeys.COPY_ITEM_CATEGORY_ACTION_SUCCESSFULLY_PERFORMED);
		context.setCompleted();
		LoggerHelper.logMethodExit(logger, COPY_ITEM_CATEGORY_HANDLER, "copyItemCategory");
	}
}


