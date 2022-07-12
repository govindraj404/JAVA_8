package com.sap.ic.cmh.returnpo.persistency;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;
import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.persistence.PersistenceService;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders;
import cds.gen.returnpurchaseorderservice.ReturnPurchaseOrders_;

@Repository
public class ReturnPurchaseOrderDao {

    private DraftService draftService;

    ReturnPurchaseOrderDao(@Qualifier("ReturnPurchaseOrderService") DraftService draftService, PersistenceService db) {
        this.draftService = draftService;
        this.db = db;
    }

    @Autowired
    PersistenceService db;

    /**
     * Get Return Purchase Order details based on Return order id
     * 
     * @param returnPurchaseOrderId
     * @return
     */
    public Result getReturnPurchaseOrderDetails(String returnPurchaseOrderId) {
        CqnSelect select = Select.from(ReturnPurchaseOrders_.class)
                .columns(ReturnPurchaseOrders.REASON_CODE, ReturnPurchaseOrders.PERSON_RESPONSIBLE_ID,
                        ReturnPurchaseOrders.PURCHASING_GROUP_CODE)
                .where(rpo -> rpo.ID().eq(returnPurchaseOrderId));
        return db.run(select);
    }

    /**
     * Get Return Purchase Order details based on Return order id
     * 
     * @param returnPurchaseOrderId
     * @return
     */
    public Result getReturnPurchaseOrderBasedOnId(String returnPurchaseOrderId) {
        CqnSelect select = Select.from(ReturnPurchaseOrders_.class).where(rpo -> rpo.ID().eq(returnPurchaseOrderId));
        return db.run(select);
    }

    /**
     * Check if Return Purchase Order exists
     */
    public Result checkIfReturnPOExistsBasedOnNumber(String returnPoNumber) {
        CqnSelect select = Select.from(ReturnPurchaseOrders_.class).columns(ReturnPurchaseOrders.ID)
                .where(rpo -> rpo.identifier().eq(returnPoNumber));
        return db.run(select);
    }

    /**
     * Get Return Purchase Order details based on Complaint id
     */
    public Result getReturnPurchaseOrderDetailsBasedOnComplaintId(String complaintId) {
        CqnSelect select = Select.from(ReturnPurchaseOrders_.class).columns(ReturnPurchaseOrders.ID)
                .where(rpo -> rpo.complaint_ID().eq(complaintId));
        return db.run(select);
    }

    /**
     * Get Return Order Status and Company code based on ID
     */
    public Result getReturnOrderStatusAndCompanyCode(String returnPurchaseOrderId) {
        CqnSelect select = Select.from(ReturnPurchaseOrders_.class)
                .columns(ReturnPurchaseOrders.STATUS_CODE, ReturnPurchaseOrders.COMPANY_ID)
                .where(rpo -> rpo.ID().eq(returnPurchaseOrderId));
        return db.run(select);
    }

    /**
     * Read draft Return Order based on Complaint ID
     * 
     * @param complaintId
     * @return Return Order
     */
    public Result getDraftReturnOrderByComplaintID(String complaintId) {
        return draftService.run(Select.from(ReturnPurchaseOrders_.class)
                .where(b -> b.get(ReturnPurchaseOrders.COMPLAINT_ID).eq(complaintId)
                        .and(b.IsActiveEntity().eq(false))));
    }

    /**
     * Delete draft ReturnOrder based on QN ID
     * 
     * @param returnPurchaseOrderId
     */
    public void deleteDraftReturnOrderByID(String returnPurchaseOrderId) {
        draftService.cancelDraft(
                Delete.from(ReturnPurchaseOrders_.class).where(b -> b.ID().eq(returnPurchaseOrderId)));
    }
    
    /**
     * Get Return Order Status and Company code based on ID
     */
    public Result getActiveReturnPurchaseOrders(String returnPurchaseOrderId) {
        CqnSelect select = Select.from(cds.gen.managereturnpurchaseorderservice.ReturnPurchaseOrders_.class)
                .where(rpo -> rpo.ID().eq(returnPurchaseOrderId));
        return db.run(select);
    }

}