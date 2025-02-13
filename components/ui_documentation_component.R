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

# Content sections as separate variables for better maintainability
overview_content <- "
  <p>The Debt Path Explorer helps analyze how policy changes could affect a country's debt sustainability. It uses the IMF's debt dynamics framework to project how debt-to-GDP ratios evolve under different scenarios.</p>
  
  <h5>How It Works</h5>
  
  <h6><b>1. Starting Point: IMF WEO Baseline</b></h6>
  <p>We begin with data from the IMF's World Economic Outlook (WEO), which provides:</p>
  <ul>
    <li>GDP growth projections</li>
    <li>Primary fiscal balance forecasts</li>
    <li>Government debt levels</li>
  </ul>
  
  <h6><b>2. Policy Adjustments</b></h6>
  <p>Users can model policy changes by adjusting three key variables:</p>
  <ul>
    <li>GDP growth rate</li>
    <li>Real effective interest rate</li>
    <li>Primary fiscal balance</li>
  </ul>
  
  <p>For example, you might model how a new carbon credit program could improve the primary balance by 0.2% of GDP annually.</p>
  
  <h6><b>3. Resulting Projections</b></h6>
  <p>The tool combines baseline values with policy adjustments to show how debt trajectories might change. This helps answer questions like:</p>
  <ul>
    <li>How much could sustainability initiatives improve debt sustainability?</li>
    <li>What impact would lower borrowing costs from green bonds have?</li>
    <li>How do changes in growth, interest rates, and fiscal balances interact?</li>
  </ul>
  <h5>Important Notes</h5>
  <ul>
    <li>The tool provides a simplified but rigorous starting point for policy analysis</li>
    <li>Results should be interpreted as illustrative projections, not precise forecasts</li>
    <li>For detailed country analysis, this tool should complement rather than replace full DSA frameworks</li>
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
  <p>The <code>imfweo</code> R package, developed by <a href='https://github.com/Teal-Insights/imfweo' target='_blank'>Teal-Insights</a>, represents a significant advancement in accessing and analyzing IMF World Economic Outlook (WEO) data. This package streamlines the process of retrieving and working with WEO data directly within the R programming environment, making it particularly valuable for economists, researchers, and policy analysts. The package offers functionality to download data from various WEO releases, handle multiple variables across different countries and time periods, and process the data into formats suitable for analysis. Despite being a minimum viable product under active development, it already provides essential features such as automatic data updating, efficient data transformation, and compatibility with common R data manipulation packages. The package's architecture is designed to accommodate future enhancements while maintaining a user-friendly interface that simplifies the often complex task of working with international economic data. For researchers conducting debt dynamics analysis or broader macroeconomic studies, <code>imfweo</code> serves as a valuable tool that reduces the technical barriers to accessing and utilizing IMF WEO data.</p>

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
      create_section_card("Overview", overview_content),
      create_section_card("Key Equation", key_equation_content),
      create_section_card("Methodology", methodology_content)
    )
  )
}

# ends: -------------------------------------------------------------------
