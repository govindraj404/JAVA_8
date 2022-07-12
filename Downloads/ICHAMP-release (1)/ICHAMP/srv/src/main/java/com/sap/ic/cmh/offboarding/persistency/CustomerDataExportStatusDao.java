package com.sap.ic.cmh.offboarding.persistency;

import cds.gen.com.sap.ic.cmh.customerdataexportstatus.CustomerDataExportStatuses;
import cds.gen.com.sap.ic.cmh.customerdataexportstatus.CustomerDataExportStatuses_;
import com.sap.cds.ql.Insert;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.Update;
import com.sap.cds.ql.cqn.CqnInsert;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.ql.cqn.CqnUpdate;
import com.sap.cds.services.persistence.PersistenceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class CustomerDataExportStatusDao {
    @Autowired
    PersistenceService db;

    public CustomerDataExportStatuses createCustomerDataExportStatus(CustomerDataExportStatuses customerDataExportStatuses) {
        CqnInsert cqnInsert = Insert.into(CustomerDataExportStatuses_.class).entry(customerDataExportStatuses);
        return db.run(cqnInsert).first(CustomerDataExportStatuses.class).get();
    }

    public void updateCustomerDataExportStatus(CustomerDataExportStatuses customerDataExportStatuses) {
        CqnUpdate cqnUpdate = Update.entity(CustomerDataExportStatuses_.class).data(customerDataExportStatuses);
        db.run(cqnUpdate);
    }

    public CustomerDataExportStatuses getCustomerDataExportStatus(String id) {
        CqnSelect cqnSelect = Select.from(CustomerDataExportStatuses_.class)
                .where(customerDataExportStatuses -> customerDataExportStatuses.ID().eq(id));
        return db.run(cqnSelect).first(CustomerDataExportStatuses.class).get();
    }
}
