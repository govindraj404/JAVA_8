package com.sap.ic.cmh.managecomplaint.actions.handlers;

import cds.gen.complaintservice.DiscardContext;
import cds.gen.complaintservice.ComplaintService_;
import cds.gen.complaintservice.Complaints;
import cds.gen.complaintservice.Complaints_;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.cds.CdsService;
import com.sap.cds.services.handler.EventHandler;
import com.sap.cds.services.handler.annotations.On;
import com.sap.cds.services.handler.annotations.ServiceName;
import com.sap.cds.services.messages.Messages;
import com.sap.ic.cmh.businessobjects.persistency.BusinessObjectDao;
import com.sap.ic.cmh.complaint.persistency.ComplaintsDao;
import com.sap.ic.cmh.complaint.service.StreamService;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

@RequestScope
@Component
@ServiceName(ComplaintService_.CDS_NAME)
public class DiscardComplaintHandler implements EventHandler {
    @Autowired
    Messages messages;
    @Autowired
    ComplaintsDao complaintsDao;
    @Autowired
    StreamService streamService;

    @Autowired
	BusinessObjectDao businessObjectDao;

    public static final Logger logger = LoggerHelper.getLogger(DiscardComplaintHandler.class);

    @On(entity = Complaints_.CDS_NAME)
    public void discard(DiscardContext context) {
        logger.info("Entered Complaint Discard Context");
        String sMessageKey = "";
        CqnSelect select = context.getCqn();
        Complaints complaints = ((CdsService) context.getService()).run(select)
                .single(Complaints.class);
        if (Boolean.TRUE.equals(complaints.getIsActiveEntity())) {
            if (complaints.getComplaintStatusCode().equals(Constants.COMPLAINT_CREATED_STATUS)) {
                complaints.setComplaintStatusCode(Constants.COMPLAINT_DISCARDED);
                /* START of setting the values of virtual fields based on conditions */
                complaints.setIsUpdateRestricted(true);
                complaints.setIsHideAdaptStreams(true);
                complaints.setIsHideDiscardComplaint(true);
                /* END of setting the values of virtual fields based on conditions */
                complaintsDao.updateComplaint(complaints);
                sMessageKey = MessageKeys.COMPLAINT_DISCARD_ACTION_SUCCESSFULLY_PERFORMED;
            } else {
                sMessageKey = MessageKeys.COMPLAINT_DISCARD_ACTION_NOT_SUCCESSFULLY_PERFORMED_WHEN_BOS_ARE_ACTIVE;
            }
        } else {
            sMessageKey = MessageKeys.COMPLAINT_DISCARD_ACTION_NOT_SUCCESSFULLY_PERFORMED_FOR_DRAFT_RECORD;
        }
        messages.success(sMessageKey);
        context.setResult(complaints);
        context.setCompleted();
    }
}

