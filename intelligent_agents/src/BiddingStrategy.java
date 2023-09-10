import genius.core.Bid;
import genius.core.BidHistory;
import genius.core.bidding.BidDetails;
import genius.core.issue.Issue;
import genius.core.issue.ValueDiscrete;
import genius.core.parties.AbstractNegotiationParty;
import genius.core.utility.EvaluatorDiscrete;
import genius.core.utility.UtilitySpace;

import java.util.*;
import java.util.stream.Collectors;

public class BiddingStrategy {
    private Agent14 agent;
    private final UtilitySpace utilitySpace;
    private List<Bid> bidHistory;
    private double pastTarget;
    private double maxUtil;
    private double reservation;
    private int turn;
    private List<BidDetails> bestBids;
    private OpponentModel opponent;
    private int bidsToGenerate = 100;
    private int bestBidsLimit = 100;
    
    

    public BiddingStrategy(Agent14 agent, UtilitySpace us, List<Bid> bh, double maxUtil, double reservation, OpponentModel opponent) {
        this.agent = agent;
        this.utilitySpace = us;
        this.bidHistory = bh;
        this.pastTarget = maxUtil;
        this.maxUtil = maxUtil;
        this.reservation = reservation; // does the reservation change?
        this.turn = 0;
        this.bestBids = new ArrayList<>();
        this.opponent = opponent;
        System.out.println("Reservation:" + reservation);
    }

    public double getConcession() {
        Bid lastBid = bidHistory.get(bidHistory.size() - 1);
        Bid previousOffer = bidHistory.get(bidHistory.size() - 2);

        return (utilitySpace.getUtility(lastBid) - utilitySpace.getUtility(previousOffer));
    }

    public double getNewTarget() {
        double concession = bidHistory.size() < 2 ? 0 : getConcession();
//        double weightC = (double) (1 - (1 / bidHistory.size()));// utilitySpace.getDomain().getNumberOfPossibleBids();
//        double roundConc = concession * weightC;
        double target = pastTarget - concession;

        target = Math.max(reservation, target); // reservation value
        target = Math.min(maxUtil, target); // sometimes the opponent gives us a better offer instead. and may overflow util 1.

        System.out.println();
        System.out.println(target);
        System.out.println("Concession:" + concession);
        pastTarget = target;
        return target;
    }

    public double getTargetUtility() {
        return pastTarget;
    }


    /**
     * @desc creates a bid to propose following bidding strategy
     * @return bid to propose
     */
    public Bid createBid() {
        turn++;

        // every 10 rounds the nash products of
        // the best bids are updated
        if (turn % 10 == 0) {
            for (BidDetails bid : bestBids) bid.setMyUndiscountedUtil(nashProduct(bid.getBid()));
        }

        // each round 100 bids are generated
        // the bid with the best nash product is
        // stored in bestbids
        double minTarget = getNewTarget();
        Bid bestBid = agent.generateRandomBidAboveTarget(minTarget);
        double maxNashProduct = nashProduct(bestBid);
        for (int i = 0; i < bidsToGenerate; i++) {
            Bid randomBid = agent.generateRandomBidAboveTarget(minTarget);
            double nashProduct = nashProduct(randomBid);
            if (nashProduct > maxNashProduct) {maxNashProduct = nashProduct; bestBid = randomBid;}
        }

        if (bestBids.size() < bestBidsLimit) bestBids.add(new BidDetails(bestBid, maxNashProduct));
        else {
            // if the bestbids list is full
            // the worst bid in bestbids is compared
            // to the best generated bid this round
            // the best takes the spot in the list
            Collections.sort(bestBids, Comparator.comparingDouble(BidDetails::getMyUndiscountedUtil));
            BidDetails worstBid = bestBids.get(0);

            if (maxNashProduct > worstBid.getMyUndiscountedUtil()) {
                bestBids.remove(0);
                bestBids.add(new BidDetails(bestBid, maxNashProduct));
                Collections.sort(bestBids, Comparator.comparingDouble(BidDetails::getMyUndiscountedUtil));
            }

            // after 100 rounds the number of bids to
            // generate and the size of the bestbids list
            // is reduced by 5 until each is 10
            if (turn != 100 && turn % 100 == 0) {
                if (bestBids.size()>10) for (int i=0;i<5;i++) bestBids.remove(0);
                if (bidsToGenerate>10) for (int i=0;i<5;i++) bidsToGenerate -= 5;
            }

            // one of top 5 best bids is proposed as offer
            List<BidDetails> top5 = bestBids.stream().skip(bestBids.size()-5).collect(Collectors.toList());
            Random random = new Random();
            bestBid = top5.get(random.nextInt(5)).getBid();
        }

        return bestBid;
    }


    /**
     * @desc calculates nash product of provided bid
     * @param bid bid used to calculate nash product
     * @return nash product of provided bid
     */
    public double nashProduct(Bid bid) {
        double np = agent.getUtility(bid);
        np *= opponent.getOpponentUtility(bid);
        return np;
    }
}

