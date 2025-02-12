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
  <p>The projection of debt-to-GDP ratio using the debt dynamics equation provides a systematic framework for analyzing how public debt evolves over time under different macroeconomic conditions and policy scenarios. This approach begins with baseline values derived from historical data and IMF World Economic Outlook projections, which include the real effective interest rate, GDP growth rate, and primary balance. These baseline values represent the starting point for understanding the expected trajectory of public debt under current conditions and policies.</p>

  <p>To assess the impact of various policy interventions or economic shocks, the framework incorporates policy shock values that can be applied to each of the key variables. These shock values represent deviations from the baseline scenario and can be positive or negative, allowing for the analysis of both favorable and adverse scenarios. For instance, policymakers might want to examine how an increase in interest rates, a slowdown in economic growth, or a change in fiscal policy affecting the primary balance would impact debt sustainability.</p>

  <p>The final shock values are then calculated by combining the baseline values with their respective policy shock values. These final values capture the total effect of both the existing economic conditions and the proposed policy changes or external shocks. By inputting these final shocked values into the debt dynamics equation, analysts can generate projections of the debt-to-GDP ratio that reflect the combined impact of baseline conditions and policy interventions. This comprehensive approach enables policymakers to evaluate different policy options and their implications for debt sustainability, making it an invaluable tool for fiscal policy planning and debt management strategies. The resulting projections help identify potential risks to debt sustainability and inform decisions about necessary policy adjustments to maintain debt at sustainable levels.</p>"

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
  <p>The computation process for debt dynamics analysis is fundamentally grounded in three essential variables obtained from the IMF World Economic Outlook database: Gross domestic product at current prices, General government primary net lending/borrowing (primary balance), and General government gross debt. This comprehensive framework enables researchers and policymakers to analyze the evolution of public debt over time, taking into account the complex interplay between economic growth, interest rates, and fiscal policy decisions. The methodology's strength lies in its ability to decompose debt dynamics into its constituent components, allowing for a detailed understanding of how different macroeconomic factors contribute to changes in the debt-to-GDP ratio.</p>

  <h5>An imfweo R package</h5>
  <p>The <code>imfweo</code> R package, developed by <a href='https://github.com/Teal-Insights/imfweo' target='_blank'>Teal-Insights</a>, represents a significant advancement in accessing and analyzing IMF World Economic Outlook (WEO) data. This package streamlines the process of retrieving and working with WEO data directly within the R programming environment, making it particularly valuable for economists, researchers, and policy analysts. The package offers functionality to download data from various WEO releases, handle multiple variables across different countries and time periods, and process the data into formats suitable for analysis. Despite being a minimum viable product under active development, it already provides essential features such as automatic data updating, efficient data transformation, and compatibility with common R data manipulation packages. The package's architecture is designed to accommodate future enhancements while maintaining a user-friendly interface that simplifies the often complex task of working with international economic data. For researchers conducting debt dynamics analysis or broader macroeconomic studies, <code>imfweo</code> serves as a valuable tool that reduces the technical barriers to accessing and utilizing IMF WEO data.</p>

  <h5>Key Equations:</h5>
  <h6>1. Main Debt Dynamics Equation:</h6>
  \\[ \\tag{1} d_t = \\frac{1 + r_t}{1 + g_t}d_{t-1} - pb_t \\]

  <h6>2. Real Effective Interest Rate Derivation:</h6>
  \\[ \\tag{2} r_t = \\frac{(d_t + pb_t)(1 + g_t)}{d_{t-1}} - 1 \\]

  <h6>3. Final Shock Calculations:</h6>
  \\[ \\tag{3a} r_t^{final} = r_t^{baseline} + r_t^{shock} \\]
  \\[ \\tag{3b} g_t^{final} = g_t^{baseline} + g_t^{shock} \\]
  \\[ \\tag{3c} pb_t^{final} = pb_t^{baseline} + pb_t^{shock} \\]

  <h5>Where:</h5>
  <ul>
    <li>\\( d_t \\) = public-debt-to-GDP ratio at time t</li>
    <li>\\( r_t \\) = real effective interest rate</li>
    <li>\\( g_t \\) = real GDP growth rate</li>
    <li>\\( pb_t \\) = primary-balance-to-GDP ratio</li>
    <li>\\( GDP_t \\) = Gross Domestic Product at time t</li>
    <li>\\( r_t^{final}, g_t^{final}, pb_t^{final} \\) = final shocked values</li>
    <li>\\( r_t^{baseline}, g_t^{baseline}, pb_t^{baseline} \\) = baseline values</li>
    <li>\\( r_t^{shock}, g_t^{shock}, pb_t^{shock} \\) = policy shock values</li>
  </ul>

  <h5>Computation of Real Effective Interest Rate:</h5>
  <p>The real effective interest rate (\\(r_t\\)) computation, represented in equation (2), employs a backward calculation approach utilizing the debt dynamics equation. This derivation is made possible by having access to actual debt levels, calculated GDP growth rates, and primary balances from historical data. The methodology involves rearranging the original debt dynamics equation to isolate the interest rate term, thereby revealing the implicit rate that would have generated the observed changes in debt ratios. This approach captures the effective cost of borrowing faced by the government across its entire debt portfolio, incorporating various maturities, currencies, and interest rate structures.</p>

  <h5>Debt Projection:</h5>
  <p>The final projection methodology incorporates policy shocks through equations (3a), (3b), and (3c), where baseline values for real effective interest rates, GDP growth, and primary balance are adjusted by user-specified shock values. These shock calculations are performed in percentage terms, with the final shocked values representing the sum of baseline and policy shock values for each respective variable. The resulting final values (\\(r_t^{final}, g_t^{final}, pb_t^{final}\\)) are then input into the main debt dynamics equation (1) to generate the debt projection under the specified shock scenario. This approach allows for a comprehensive analysis of how policy changes or external shocks might affect debt sustainability, providing policymakers with valuable insights for debt management and fiscal policy decisions.</p>"

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
