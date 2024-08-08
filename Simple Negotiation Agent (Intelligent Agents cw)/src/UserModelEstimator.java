
import genius.core.Bid;
import genius.core.issue.Issue;
import genius.core.issue.Value;
import genius.core.issue.ValueDiscrete;
import genius.core.uncertainty.BidRanking;
import genius.core.utility.AdditiveUtilitySpace;
import genius.core.utility.EvaluatorDiscrete;
import gurobi.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class UserModelEstimator {

    public static void estimate(AdditiveUtilitySpace utilitySpace, BidRanking ranking) {
        try {
            // Setup Initial Environment
            final GRBEnv env = new GRBEnv(true);
            env.set(GRB.IntParam.OutputFlag, 0);
            env.start();

            // Create Empty GRB Model
            final GRBModel solver = new GRBModel(env);

            // Get an LP variable for each discrete value X across the bids whose preference we know
            final Map<Issue, Map<Value, GRBVar>> variables = getLPVariables(solver, ranking);

            // Add Epsilon Variables
            final List<Bid> orderedBids = ranking.getBidOrder();
            final int numberOfConstraints = orderedBids.size() - 1;
            final GRBVar[] epsilons = getEpsilonVariables(solver, numberOfConstraints);

            // Define an objective function that maximises the sum of epsilons
            final GRBLinExpr objectiveExpr = getObjectiveFunction(numberOfConstraints, epsilons);
            solver.setObjective(objectiveExpr, GRB.MAXIMIZE);

            // Add LP Constraints: Outcome(i) - Outcome(i-1) - epsilon(i-1) >= 0
            addLPConstraints(solver, numberOfConstraints, epsilons, orderedBids, variables);

            final Bid lowestBid = orderedBids.get(0);
            GRBLinExpr lowestBidConstraint = new GRBLinExpr();
            for (Issue issue : lowestBid.getIssues())
                lowestBidConstraint.addTerm(1, variables.get(issue).get(lowestBid.getValue(issue)));
            solver.addConstr(lowestBidConstraint, GRB.GREATER_EQUAL, 0, "lowestBidConstraint");

            final Bid bestBid = orderedBids.get(orderedBids.size() - 1);
            GRBLinExpr bestBidConstraint = new GRBLinExpr();
            for (Issue issue : bestBid.getIssues())
                bestBidConstraint.addTerm(1, variables.get(issue).get(bestBid.getValue(issue)));
            solver.addConstr(1, GRB.GREATER_EQUAL, bestBidConstraint, "bestBidConstraint");

            // Find Solution
            solver.optimize();

            // Update Weights
            updateWeights(utilitySpace, variables);

            // Clean up
            solver.dispose();
            env.dispose();

        } catch (GRBException e) {
            System.out.println("Error code: " + e.getErrorCode() + ". " + e.getMessage());
        }
    }

    private static Map<Issue, Map<Value, GRBVar>> getLPVariables(GRBModel solver, BidRanking ranking) throws GRBException {
        final Map<Issue, Map<Value, GRBVar>> variables = new HashMap<>();
        for (Bid bid : ranking.getBidOrder()) {
            final List<Issue> issues = bid.getIssues();

            for (Issue issue : issues) {
                if (!variables.containsKey(issue)) {
                    variables.put(issue, new HashMap<>());
                }
                final Map<Value, GRBVar> variableMapping = variables.get(issue);
                final Value value = bid.getValue(issue);

                if (variableMapping != null && !variableMapping.containsKey(value)) {
                    final GRBVar variable = solver.addVar(0, 1, 0, GRB.CONTINUOUS, "variable" + Math.random());
                    variableMapping.put(value, variable);
                }
            }
        }
        return variables;
    }

    private static GRBVar[] getEpsilonVariables(GRBModel solver, int numberOfConstraints) throws GRBException {
        final GRBVar[] epsilons = new GRBVar[numberOfConstraints];
        for (int i = 0; i < numberOfConstraints; i++) {
            epsilons[i] = solver.addVar(0, 1, 0, GRB.CONTINUOUS, "epsilon" + i);
        }
        return epsilons;
    }

    private static GRBLinExpr getObjectiveFunction(int numberOfConstraints, GRBVar[] epsilons) throws GRBException {
        final GRBLinExpr objectiveExpr = new GRBLinExpr();
        for (int i = 0; i < numberOfConstraints; i++) {
            objectiveExpr.addTerm(1, epsilons[i]);
        }
        return objectiveExpr;
    }

    private static void addLPConstraints(GRBModel solver, int numberOfConstraints, GRBVar[] epsilons, List<Bid> orderedBids, Map<Issue, Map<Value, GRBVar>> vars) throws GRBException {
        for (int i = numberOfConstraints; i > 0; i--) {
            final Bid biggerBid = orderedBids.get(i);
            final Bid smallerBid = orderedBids.get(i - 1);

            GRBLinExpr constraint = new GRBLinExpr();
            for (Issue issue : biggerBid.getIssues())
                constraint.addTerm(1, vars.get(issue).get(biggerBid.getValue(issue)));


            for (Issue issue : smallerBid.getIssues()) {
                constraint.addTerm(-1, vars.get(issue).get(smallerBid.getValue(issue)));
            }

            constraint.addTerm(-1, epsilons[i - 1]);
            solver.addConstr(constraint, GRB.GREATER_EQUAL, 0, "constraint" + i);
        }
    }

    private static void updateWeights(AdditiveUtilitySpace utilitySpace, Map<Issue, Map<Value, GRBVar>> variables) throws GRBException {
        for (Issue issue : variables.keySet()) {
            final EvaluatorDiscrete issueEvaluator = (EvaluatorDiscrete) utilitySpace.getEvaluator(issue.getNumber());
            for (Map.Entry<Value, GRBVar> entry : variables.get(issue).entrySet()) {
                final Value value = entry.getKey();
                final GRBVar variable = entry.getValue();
                final double utility = Math.max(variable.get(GRB.DoubleAttr.X), 0);
                issueEvaluator.setEvaluationDouble((ValueDiscrete) value, utility);
            }
            issueEvaluator.normalizeAll();
            issueEvaluator.scaleAllValuesFrom0To1();
        }
        utilitySpace.normalizeWeights();
    }
}