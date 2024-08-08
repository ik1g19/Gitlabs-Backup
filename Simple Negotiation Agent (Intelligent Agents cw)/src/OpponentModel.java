
import genius.core.Bid;
import genius.core.BidHistory;
import genius.core.bidding.BidDetails;
import genius.core.issue.Issue;
import genius.core.issue.Value;
import genius.core.issue.ValueDiscrete;
import genius.core.utility.EvaluatorDiscrete;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class OpponentModel {

    private BidHistory bidHistory;

    private IssueContainer[] issues;

    private Integer maxFrequency = 1;

    /**
     * @desc container class for issues
     */
    private class IssueContainer {
        private Issue l;              // issue
        private EvaluatorDiscrete r;  // discrete evaluator for issue

        // maintains how many times each issue option is proposed
        // by the opponent
        private HashMap<ValueDiscrete,Integer>optionFrequency;

        public IssueContainer(Issue l, EvaluatorDiscrete r){
            this.l = l;
            this.r = r;

            optionFrequency = new HashMap<>();
        }
        public Issue getIssue(){ return l; }
        public EvaluatorDiscrete getEvaluator(){ return r; }
        public void setIssue(Issue l){ this.l = l; }
        public void setEvaluator(EvaluatorDiscrete r){ this.r = r; }
        public List<Integer> getFrequencies() {return new ArrayList<>(optionFrequency.values());}
        public List<ValueDiscrete> getOptions() {return new ArrayList<>(optionFrequency.keySet());}
        public Integer getFrequency(ValueDiscrete option) {return optionFrequency.get(option);}

        /**
         * @desc increases frequency of proposed option
         * @param option option which was just proposed
         */
        public void incrementOptionFreq(ValueDiscrete option) {
            if (optionFrequency.containsKey(option)) {
                optionFrequency.put(option, optionFrequency.get(option) + 1);
                if (optionFrequency.get(option) > maxFrequency) maxFrequency = optionFrequency.get(option);
            }
            else optionFrequency.put(option, 1);
        }
    }



    public OpponentModel(Bid setupBid) {
        this.bidHistory = new BidHistory();

        int numberOfIssues = setupBid.getIssues().size();
        issues = new IssueContainer[numberOfIssues];

        for (int i = 0; i< numberOfIssues; i++) {
            issues[i] = new IssueContainer(setupBid.getIssues().get(i),new EvaluatorDiscrete());
        }
    }


    /**
     * @desc estimates opponents utility of provided bid
     * @param bid bid to estimate opponents utility of
     * @return opponent's estimated utility of bid
     */
    public double getOpponentUtility(Bid bid) {
        double utility = 0.0;

        HashMap<Integer, Value> issueAndValue = bid.getValues();

        for (IssueContainer issue : issues) {
            EvaluatorDiscrete eval = issue.getEvaluator();
            Issue i = issue.getIssue();

            double weight = eval.getWeight();
            ValueDiscrete option = (ValueDiscrete) issueAndValue.get(i.getNumber());

            if (eval.getValues().contains(option)) utility += weight * eval.getDoubleValue(option);
        }

        return utility;
    }


    /**
     * @desc add bid to list of previous bids
     * @param bid opponent's latest bid
     */
    public void addBidToHistory(Bid bid) {
        bidHistory.add(new BidDetails(bid, 0));

        for (IssueContainer i : issues) {
            ValueDiscrete option = (ValueDiscrete) bid.getValues().get(i.getIssue());
            i.incrementOptionFreq(option);
        }

        setWeights();
    }


    /**
     * @desc adjusts weights of issues
     */
    private void setWeights() {
        double totalWeight = 0.0;

        for (IssueContainer i : issues) {
            double weight = 0.0;
            for (Integer freq : i.getFrequencies()) {
                weight += Math.pow(freq, 2) / Math.pow(bidHistory.size(), 2);
            }
            i.getEvaluator().setWeight(weight);
            totalWeight += weight;
        }

        for (IssueContainer i : issues) {
            double weight = i.getEvaluator().getWeight();
            i.getEvaluator().setWeight(weight/totalWeight);

            for (ValueDiscrete option : i.getOptions()) {
                i.getEvaluator().setEvaluationDouble(option, i.getFrequency(option) / maxFrequency);
            }
        }
    }

}
