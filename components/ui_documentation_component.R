# starts: -----------------------------------------------------------------
# Helper function to create a section card
create_section_card <- function(title, content) {
  card(
    class = "mt-4",
    card_header(class = "bg-light", h4(title)),
    card_body(
      withMathJax(
        HTML(sprintf("<div class='math-section'>%s</div>", content))
      )
    )
  )
}

# Content sections
overview_content <- "
  <h5>Development Status</h2>
  <p>
    The Debt Path Explorer is currently in prototype stage. This means:
  </p>
  <ul>
    <li><strong>Simplified Modeling</strong>: The tool uses standard debt dynamics equations with real WEO data but employs simplified transmission channels</li>
    <li><strong>Illustrative Projections</strong>: Results should be interpreted as directional scenarios rather than precise forecasts</li>
    <li><strong>Conceptual Workflow</strong>: The current version demonstrates a user journey focused on understanding key debt drivers</li>
  </ul>
  <h5>Assessment Framework</h2>
  <p>
    The Debt Path Explorer serves as a starting point for policy analysis that:
  </p>
  <ul>
    <li><strong>Uses IMF WEO data</strong> as a baseline for projections</li>
    <li><strong>Allows users to model policy shocks</strong> to key debt dynamics variables</li>
    <li><strong>Visualizes alternative debt trajectories</strong> based on user inputs</li>
    <li><strong>Provides an accessible entry point</strong> to more sophisticated debt analysis</li>
  </ul>
  <h5>Development Roadmap</h2>
  <p>
    This tool represents the first step in a multi-phase approach:
  </p>
  <ul>
    <li><strong>Current phase</strong>: Functional prototype with real data and basic scenario modeling</li>
    <li><strong>Next phase</strong>: Enhanced transmission channels with empirically calibrated relationships</li>
    <li><strong>Future phase</strong>: Integration with more comprehensive DSA frameworks</li>
    <li><strong>Final phase</strong>: Expanded country coverage and macroeconomic variables</li>
  </ul>
  <h5>Understanding the Results</h2>
  <p>
    Results from this tool should be interpreted as:
  </p>
  <ul>
    <li><strong>Directional guidance</strong> on how policy changes might affect debt trajectories</li>
    <li><strong>Starting points</strong> for more detailed country-specific analysis</li>
    <li><strong>Relative comparisons</strong> between baseline and alternative scenarios</li>
  </ul>
  <h5>Providing Feedback</h2>
  <p>
    Your insights are crucial to ensuring this tool evolves to meet analytical needs. We're particularly interested in:
  </p>
  <ul>
    <li>Is the conceptual workflow intuitive and valuable?</li>
    <li>What additional variables or features would make this tool more useful?</li>
    <li>How could this tool complement your existing analytical processes?</li>
    <li>What barriers might prevent adoption by debt management offices?</li>
  </ul>
"

key_equation_content <- "
  <h5>Debt Dynamics Equation:</h5>
  \\[ d_t = \\frac{(1 + r_t)}{(1 + g_t)}d_{t-1} - pb_t \\]

  <h5>Where:</h5>
  <ul>
    <li>\\( d_t \\) = public-debt-to-GDP ratio at time t</li>
    <li>\\( r_t \\) = real effective interest rate</li>
    <li>\\( g_t \\) = real GDP growth rate</li>
    <li>\\( pb_t \\) = primary-balance-to-GDP ratio</li>
    <li>\\( t \\) = time period</li>
  </ul>"

methodology_content <- "
  <p>The computation process for debt dynamics analysis is fundamentally grounded in three essential variables obtained from the IMF World Economic Outlook database: Gross domestic product, constant prices, General government primary net lending/borrowing (primary balance), and General government gross debt. This comprehensive framework enables researchers and policymakers to analyze the evolution of public debt over time, taking into account the complex interplay between economic growth, interest rates, and fiscal policy decisions. The methodology's strength lies in its ability to decompose debt dynamics into its constituent components, allowing for a detailed understanding of how different macroeconomic factors contribute to changes in the debt-to-GDP ratio.</p>

  <h5>An imfweo R package</h5>
  <p>The <code>imfweo</code> R package, developed by <a href='https://github.com/Teal-Insights/imfweo' target='_blank'>Teal Insights</a>, represents a significant advancement in accessing and analyzing IMF World Economic Outlook (WEO) data. This package streamlines the process of retrieving and working with WEO data directly within the R programming environment, making it particularly valuable for economists, researchers, and policy analysts. The package offers functionality to download data from various WEO releases, handle multiple variables across different countries and time periods, and process the data into formats suitable for analysis. Despite being a minimum viable product under active development, it already provides essential features such as automatic data updating, efficient data transformation, and compatibility with common R data manipulation packages. The package's architecture is designed to accommodate future enhancements while maintaining a user-friendly interface that simplifies the often complex task of working with international economic data. For researchers conducting debt dynamics analysis or broader macroeconomic studies, <code>imfweo</code> serves as a valuable tool that reduces the technical barriers to accessing and utilizing IMF WEO data.</p>

  <h5>Key Equations:</h5>
  <h6>1. Main Debt Dynamics Equation:</h6>
  \\[ \\tag{1} d_t = \\frac{1 + r_t}{1 + g_t}d_{t-1} - pb_t \\]

  <h6>2. Real Effective Interest Rate Derivation:</h6>
  \\[ \\tag{2} r_t = \\frac{(d_t + pb_t)(1 + g_t)}{d_{t-1}} - 1 \\]

  <h6>3. Policy-Adjusted Forecast (%) Calculations:</h6>
  \\[ \\tag{3a} r_t^{\\text{Policy-Adjusted Forecast (%)}} = r_t^{\\text{IMF WEO Baseline (%)}} + r_t^{\\text{Policy shock (%)}} \\]
  \\[ \\tag{3b} g_t^{\\text{Policy-Adjusted Forecast (%)}} = g_t^{\\text{IMF WEO Baseline (%)}} + g_t^{\\text{Policy shock (%)}} \\]
  \\[ \\tag{3c} pb_t^{\\text{Policy-Adjusted Forecast (%)}} = pb_t^{\\text{IMF WEO Baseline (%)}} + pb_t^{\\text{Policy shock (%)}} \\]

  <h5>Where:</h5>
  <ul>
    <li>\\( d_t \\) = public-debt-to-GDP ratio at time t</li>
    <li>\\( r_t \\) = real effective interest rate</li>
    <li>\\( g_t \\) = real GDP growth rate</li>
    <li>\\( pb_t \\) = primary-balance-to-GDP ratio</li>
    <li>\\( GDP_t \\) = Gross Domestic Product at time t</li>
    <li>\\( r_t^{\\text{Policy-Adjusted Forecast (%)}}, g_t^{\\text{Policy-Adjusted Forecast (%)}}, pb_t^{\\text{Policy-Adjusted Forecast (%)}} \\) = Policy-Adjusted Forecast (%) values</li>
    <li>\\( r_t^{\\text{IMF WEO Baseline (%)}}, g_t^{\\text{IMF WEO Baseline (%)}}, pb_t^{\\text{IMF WEO Baseline (%)}} \\) = IMF WEO Baseline (%) values</li>
    <li>\\( r_t^{\\text{Policy shock (%)}}, g_t^{\\text{Policy shock (%)}}, pb_t^{\\text{Policy shock (%)}} \\) = Policy shock (%) values</li>
  </ul>

  <h5>Computation of Real Effective Interest Rate:</h5>
  <p>The real effective interest rate (\\(r_t\\)) computation, represented in equation (2), employs a backward calculation approach utilizing the debt dynamics equation. This derivation is made possible by having access to actual debt levels, calculated GDP growth rates, and primary balances from historical data. The methodology involves rearranging the original debt dynamics equation to isolate the interest rate term, thereby revealing the implicit rate that would have generated the observed changes in debt ratios. This captures the effective cost of borrowing faced by the government across its entire debt portfolio.</p>

  <h5>Debt Projection:</h5>
  <p>The final projection methodology incorporates policy shocks through equations (3a), (3b), and (3c), where IMF WEO Baseline (%) values for real effective interest rates, GDP growth, and primary balance are adjusted by user-specified shock values. These shock calculations are performed in percentage terms, with the final shocked values representing the sum of IMF WEO Baseline (%) and policy shock values for each respective variable. The resulting final values (\\(r_t^{\\text{Policy-Adjusted Forecast (%)}}, g_t^{\\text{Policy-Adjusted Forecast (%)}}, pb_t^{\\text{Policy-Adjusted Forecast (%)}}\\)) are then input into the main debt dynamics equation (1) to generate the debt projection under the specified shock scenario.</p>"

# Main component function
ui_documentation_component <- function() {
  card_body(
    div(
      class = "container",
      create_section_card("About This Prototype", overview_content),
      create_section_card("Key Equation", key_equation_content),
      create_section_card("Methodology", methodology_content)
    )
  )
}

# ends: -------------------------------------------------------------------
