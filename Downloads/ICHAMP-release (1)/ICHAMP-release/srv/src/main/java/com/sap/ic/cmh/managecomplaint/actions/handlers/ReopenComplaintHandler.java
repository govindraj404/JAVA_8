package com.sap.ic.cmh.managecomplaint.actions.handlers;

import cds.gen.complaintservice.ComplaintService_;
import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Complaints_;
import cds.gen.complaintservice.ReopenContext;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

import java.util.Objects;

@RequestScope
@Component
@ServiceName(ComplaintService_.CDS_NAME)
public class ReopenComplaintHandler implements EventHandler {

    @Autowired
    Messages messages;
    @Autowired
    ComplaintsDao complaintsDao;

    public static final Logger logger = LoggerHelper.getLogger(ReopenComplaintHandler.class);

    @On(entity = Complaints_.CDS_NAME)
    public void reopen(ReopenContext context) {
        logger.info("Entered Complaint Reopen Context");
        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        Complaints complaints = ((CdsService) context.getService()).run(select)
                .single(Complaints.class);
        if (Boolean.TRUE.equals(complaints.getIsActiveEntity())) {
            if (Objects.equals(complaints.getComplaintStatusCode(), Constants.COMPLAINT_CLOSED)) {
                complaints.setComplaintStatusCode(Constants.COMPLAINT_REVISED);
                /* START of setting the values of virtual fields based on conditions */
                complaints.setIsHideReopenComplaint(true);
                complaints.setIsHideCloseComplaint(false);
                complaints.setIsUpdateRestricted(false);
                complaints.setIsHideAdaptStreams(false);
                /* END of setting the values of virtual fields based on conditions */
                complaintsDao.updateComplaint(complaints);

                sMessageKey = MessageKeys.COMPLAINT_REOPEN_ACTION_SUCCESSFULLY_PERFORMED;
            } else {
                sMessageKey = MessageKeys.COMPLAINT_REOPEN_ACTION_NOT_SUCCESSFULLY_PERFORMED;
            }
        } else {
            sMessageKey = MessageKeys.COMPLAINT_REOPEN_ACTION_NOT_SUCCESSFULLY_PERFORMED_FOR_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(complaints);
        context.setCompleted();
    }
}
