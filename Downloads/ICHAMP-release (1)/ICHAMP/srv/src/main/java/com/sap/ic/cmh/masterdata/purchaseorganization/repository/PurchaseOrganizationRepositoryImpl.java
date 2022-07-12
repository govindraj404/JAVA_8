package com.sap.ic.cmh.masterdata.purchaseorganization.repository;

import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Complaints_;
import cds.gen.masterdataservice.PurchaseOrganizations;
import cds.gen.masterdataservice.PurchaseOrganizations_;

import com.sap.cds.Result;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnDelete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Component
public class PurchaseOrganizationRepositoryImpl implements PurchaseOrganizationRepository {
    public static final Logger logger = LoggerHelper.getLogger(PurchaseOrganizationRepositoryImpl.class);

    @Autowired
    private PersistenceService db;

    @Override
    public Map<String, String> getPurchaseOrganizationMap(List<String> purchaseOrganizations) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getPurchaseOrganizationMap");
        CqnSelect purchaseOrganizationSelect = Select.from(PurchaseOrganizations_.class)
                .where(st -> st.purchaseOrganization().in(purchaseOrganizations));
        List<PurchaseOrganizations> purchaseOrganizationList = db.run(purchaseOrganizationSelect).listOf(PurchaseOrganizations.class);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getPurchaseOrganizationMap");
        return purchaseOrganizationList.stream().collect(Collectors.toMap(PurchaseOrganizations::getId, PurchaseOrganizations::getPurchaseOrganization));
    }

    @Override
    public List<String> getActiveComplaintsInPurchaseOrganization(List<String> purchaseOrganizationsId) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "getActiveComplaintsInPurchaseOrganization");
        List<String> complaintsList = new ArrayList<>();
        CqnSelect complaintsSelect = Select.from(Complaints_.class).where(b -> b.purchasingOrganization_ID().in(purchaseOrganizationsId).and(b.complaintStatus_code()
                .ne(Constants.COMPLAINT_CLOSED)));
        final List<Complaints> complaints = db.run(complaintsSelect).listOf(Complaints.class);
        complaintsList.addAll(complaints.stream().map(Complaints::getPurchasingOrganizationId).collect(Collectors.toList()));
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "getActiveComplaintsInPurchaseOrganization");
        return complaintsList;
    }

    @Override
    public void deletePurchaseOrganizationList(List<String> purchaseOrganizationList) {
        LoggerHelper.logMethodEntry(logger, this.getClass().getSimpleName(), "deletePurchaseOrganizationList");
        CqnDelete delete = Delete.from(PurchaseOrganizations_.class).where(st -> st.ID().in(purchaseOrganizationList));
        long deleteCount = db.run(delete).rowCount();
        logger.info("Records Deleted count: ", deleteCount);
        LoggerHelper.logMethodExit(logger, this.getClass().getSimpleName(), "deletePurchaseOrganizationList");
    }

	@Override
	public Result fetchPurchaseOrganization(String purchaseOrganizations) {
		return db.run(Select.from(PurchaseOrganizations_.class).columns(PurchaseOrganizations.ID)
                .where(st -> st.purchaseOrganization().in(purchaseOrganizations)));
	}
    
}
