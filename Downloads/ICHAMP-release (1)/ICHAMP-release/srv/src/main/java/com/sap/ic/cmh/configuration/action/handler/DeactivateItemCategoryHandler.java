package com.sap.ic.cmh.configuration.action.handler;


import cds.gen.configurationservice.ConfigurationService_;
import cds.gen.configurationservice.DeactivateItemCategoryContext;
import cds.gen.configurationservice.ItemCategories;
import cds.gen.configurationservice.ItemCategories_;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.auditlog.Action;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.After;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.configuration.handler.ItemCategoryHandler;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

@RequestScope
@Component
@ServiceName(ConfigurationService_.CDS_NAME)
public class DeactivateItemCategoryHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    PersistenceService db;
    @Autowired
    ItemCategoryHandler handler;

    public static final Logger logger = LoggerHelper.getLogger(DeactivateItemCategoryHandler.class);

    private static final String DEACTIVATE_ITEM_CATEGORY_HANDLER = "DeactivateItemCategoryHandler";

    /**
     * On Deactivate event of ItemCategories Object perform validations
     *
     * @public
     */
    @On(entity = ItemCategories_.CDS_NAME)
    public void deactivateItemCategory(final DeactivateItemCategoryContext context) {
        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        ItemCategories itemCategory = ((CdsService) context.getService()).run(select)
                .single(ItemCategories.class);
        if (Boolean.TRUE.equals(itemCategory.getIsActiveEntity())) {
            if (Boolean.FALSE.equals(itemCategory.getIsActive())) {
                sMessageKey = MessageKeys.ITEM_CATEGORY_DEACTIVATE_NOT_SUCCESSFUL;
            } else {
                handler.setOldAuditData(itemCategory);
                itemCategory.setIsActive(false);
                CqnUpdate update = Update.entity(ItemCategories_.class).data(itemCategory)
                        .where(b -> b.ID().eq(itemCategory.getId()));
                db.run(update);
                itemCategory.setIsInActive(true);
                sMessageKey = MessageKeys.ITEM_CATEGORY_DEACTIVATION_SUCCESSFUL;
            }
        } else {
            sMessageKey = MessageKeys.ITEM_CATEGORY_DEACTIVATE_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(itemCategory);
        context.setCompleted();
    }

    /**
     * After Deactivate event of ItemCategories Object perform validations
     *
     * @public
     */
    @After(entity = ItemCategories_.CDS_NAME)
    public void afterDeactivateItemCategory(final DeactivateItemCategoryContext context) {
        LoggerHelper.logMethodEntry(logger, DEACTIVATE_ITEM_CATEGORY_HANDLER, "afterDeactivateItemCategory");
        CqnSelect select = context.getCqn();
        ItemCategories itemCategory = ((CdsService) context.getService()).run(select)
                .single(ItemCategories.class);
        handler.logUpsert(Action.UPDATE, itemCategory);
        LoggerHelper.logMethodExit(logger, DEACTIVATE_ITEM_CATEGORY_HANDLER, "afterDeactivateItemCategory");
    }
}
