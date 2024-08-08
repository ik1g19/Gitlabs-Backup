import genius.core.AgentID;
import genius.core.Bid;
import genius.core.actions.Accept;
import genius.core.actions.Action;
import genius.core.actions.EndNegotiation;
import genius.core.actions.Offer;
import genius.core.issue.Issue;
import genius.core.issue.IssueDiscrete;
import genius.core.issue.ValueDiscrete;
import genius.core.parties.AbstractNegotiationParty;
import genius.core.parties.NegotiationInfo;
import genius.core.uncertainty.BidRanking;
import genius.core.utility.AbstractUtilitySpace;
import genius.core.utility.AdditiveUtilitySpace;
import genius.core.utility.EvaluatorDiscrete;


import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

/**
 * A simple example agent that makes random bids above a minimum target utility.
 *
 * @author Tim Baarslag
 */
public class Agent14 extends AbstractNegotiationParty {
    private static double MINIMUM_TARGET = 0.5;
    private Bid lastOffer;
    private List<Bid> bidHistory;
    private BiddingStrategy biddingStr;
    private OpponentModel opponent;

    /**
     * Initializes a new instance of the agent.
     */
    @Override
    public void init(NegotiationInfo info) {
        super.init(info);

        opponent = new OpponentModel(generateRandomBid());

        this.bidHistory = new ArrayList<>();
        biddingStr = new BiddingStrategy(this, getUtilitySpace(), bidHistory, utilitySpace.getUtility(getMaxUtilityBid()), utilitySpace.getReservationValue(), opponent);

        AbstractUtilitySpace utilitySpace = info.getUtilitySpace();
        AdditiveUtilitySpace additiveUtilitySpace = (AdditiveUtilitySpace) utilitySpace;

        List<Issue> issues = additiveUtilitySpace.getDomain().getIssues();

        for (Issue issue : issues) {
            int issueNumber = issue.getNumber();
            System.out.println(">> " + issue.getName() + " weight: " + additiveUtilitySpace.getWeight(issueNumber));

            // Assuming that issues are discrete only
            IssueDiscrete issueDiscrete = (IssueDiscrete) issue;
            EvaluatorDiscrete evaluatorDiscrete = (EvaluatorDiscrete) additiveUtilitySpace.getEvaluator(issueNumber);

            for (ValueDiscrete valueDiscrete : issueDiscrete.getValues()) {
                System.out.println(valueDiscrete.getValue());
                System.out.println("Evaluation(getValue): " + evaluatorDiscrete.getValue(valueDiscrete));
                try {
                    System.out.println("Evaluation(getEvaluation): " + evaluatorDiscrete.getEvaluation(valueDiscrete));
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }


        System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)));
        if (hasPreferenceUncertainty()) {
            utilitySpace = info.getUtilitySpace();
            additiveUtilitySpace = (AdditiveUtilitySpace) utilitySpace;
            UserModelEstimator.estimate(additiveUtilitySpace, userModel.getBidRanking());
        }
    }

    /**
     * Makes a random offer above the minimum utility target
     * Accepts everything above the reservation value at the end of the negotiation; or breaks off otherwise.
     */
    @Override
    public Action chooseAction(List<Class<? extends Action>> possibleActions) {
        Bid nextBid = biddingStr.createBid();

        // Check for acceptance if we have received an offer
        if (lastOffer != null)
            if (acceptanceStrategy(nextBid, 1.02, 0.04, 0.98))
                return new Accept(getPartyId(), lastOffer);

        if (timeline.getTime() == 1)
            return new EndNegotiation(getPartyId());

        return new Offer(getPartyId(), nextBid);
    }

    /**
     * Accepts offer if a * utility of offer + b is greater than the utility of the next bid
     * or
     * when the current offer is better than the offers in the previous time window
     *
     * @param nextBid next potential bid to send to the opponent
     * @param a       scale factor to multiply the opponents bid by
     * @param b       minimum utility gap that is sufficient to accept
     * @param t       time to add additional acceptance condition
     * @return whether to accept or not
     */
    public boolean acceptanceStrategy(Bid nextBid, double a, double b, double t) {
        if ((a * getUtility(lastOffer) + b) >= getUtility(nextBid))
            return true;

        if (timeline.getTime() >= t) {
            double remainingTime = 1 - timeline.getTime();
            int startWindow = (int) Math.ceil(bidHistory.size() * remainingTime);
            int endWindow = bidHistory.size();
            double maxUtil = 0.0;

            for (int i = startWindow - 1; i < endWindow; i++) {
                maxUtil = Math.max(getUtility(bidHistory.get(i)), maxUtil);
            }

            return getUtility(nextBid) >= maxUtil;
        }

        return false;
    }


    private Bid getMaxUtilityBid() {
        try {
            return utilitySpace.getMaxUtilityBid();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    private double getMinTargetUtility() {
        double maxBid = utilitySpace.getUtility(getMaxUtilityBid());
        double time = getTimeLine().getTime();
        return MINIMUM_TARGET + (1 - time) * (maxBid - MINIMUM_TARGET);
    }

    public Bid generateRandomBidAboveTarget(double minTarget) {
        double util = -1;
        int i = 0;
        Bid bestRandomBid = null;
        do {
             Bid newRandomBid = generateRandomBid();
             double newRandomUtil = getUtility(newRandomBid);

             if (newRandomUtil > util) {
                 bestRandomBid = newRandomBid;
             }

            util = getUtility(bestRandomBid);
        }
        while (util < minTarget && i++ < 5000);
        return bestRandomBid;
    }

    /**
     * Remembers the offers received by the opponent.
     */
    @Override
    public void receiveMessage(AgentID sender, Action action) {
        if (action instanceof Offer) {
            lastOffer = ((Offer) action).getBid();
            bidHistory.add(((Offer) action).getBid());

            opponent.addBidToHistory(lastOffer);
        }
    }

    @Override
    public String getDescription() {
        return "My birth-name is Agent14, but you can call me Mata Hari";
    }

    /**
     * This stub can be expanded to deal with preference uncertainty in a more sophisticated way than the default behavior.
     */
    @Override
    public AbstractUtilitySpace estimateUtilitySpace() {
        return super.estimateUtilitySpace();
    }
}
