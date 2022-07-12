package com.sap.ic.cmh.supplierissueprocess.persistency;

import cds.gen.supplierissueprocessservice.Supplier8DProcesses;
import cds.gen.supplierissueprocessservice.Supplier8DProcesses_;
import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.Delete;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.draft.DraftService;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;
import java.util.List;

@Repository
public class EightDDao {

	@Autowired
	PersistenceService db;

	private DraftService draftService;

	EightDDao(@Qualifier("SupplierIssueProcessService") DraftService draftService, PersistenceService db) {
		this.draftService = draftService;
		this.db = db;
	}

	/**
	 * Get supplier 8D details based on supplier8D ID
	 */
	public Result getEightDBasedOnId(String eightD) {
		CqnSelect val = Select.from(Supplier8DProcesses_.class)
				.columns(Supplier8DProcesses.ID, Supplier8DProcesses.STATUS_CODE,
						Supplier8DProcesses.SUPPLIER_ISSUE_PROCESSES_TYPE, Supplier8DProcesses.PERSON_RESPONSIBLE_ID)
				.where(b -> b.get(Supplier8DProcesses.ID).eq(eightD));
		return db.run(val);
	}

    /**
	 * Get supplier 8D details based on supplier8D ID
	 */
	public Result getEightD(String eightD) {
		CqnSelect val = Select.from(Supplier8DProcesses_.class).where(b -> b.get(Supplier8DProcesses.ID).eq(eightD));
		return db.run(val);
	}

	/**
	 * Get supplier 8D details based on complaint ID
	 */
	public Result getEightDDetailsBasedOnComplaintId(String complaintId) {
		CqnSelect val = Select.from(Supplier8DProcesses_.class)
				.where(b -> b.get(Supplier8DProcesses.COMPLAINT_ID).eq(complaintId));
		return db.run(val);
	}

	/**
	 * Get all active Supplier 8Ds
	 */
	public List<cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses> getActiveEightD() {
		CqnSelect val = Select.from(cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses_.class)
				.where(b -> b.status_code().ne(Constants.SUPPLIER_ISSUE_PROCESS_STATUS_CLOSED));
		return db.run(val).listOf(cds.gen.supplierissueprocessinternalservice.Supplier8DProcesses.class);
	}

	/**
	 * Read draft Supplier8D based on Complaint ID
	 * 
	 * @param complaintId
	 * @return Supplier8D
	 */
	public Result getDraftEightDByComplaintID(String complaintId) {
		return draftService.run(Select.from(Supplier8DProcesses_.class)
				.where(b -> b.get(Supplier8DProcesses.COMPLAINT_ID).eq(complaintId).and(b.IsActiveEntity().eq(false))));
	}

	/**
	 * Delete draft Supplier8D based on eightD ID
	 * 
	 * @param eightDId
	 */

	public void deleteDraftEightDByID(String eightDId) {
		draftService.cancelDraft(
				Delete.from(Supplier8DProcesses_.class).where(b -> b.ID().eq(eightDId)));
	}
}
