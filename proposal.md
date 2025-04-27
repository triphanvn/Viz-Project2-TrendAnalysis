# 📌 **Project Proposal | Search Trend Analysis Dashboard**

**Designed and Developed by Doan Quang Hung and Phan Minh Tri**

---

## **Introduction**

### **Overview**

With the rapid advancement of Large Language Models (LLMs) and the essential role that search engines play in our daily lives, search trends from specific countries, such as Vietnam, offer valuable insights that can benefit a variety of stakeholders. These insights can inform decision-makers, businesses, researchers, and the general public about current trends and hot topics. To provide a deeper understanding of these trends, we propose the development of an interactive dashboard that leverages the Google Trends API to compare the popularity of different keywords over time. 

This tool will enable users to explore how search interest for selected keywords evolves, providing insights into public interest trends. By incorporating LLMs, we aim to offer **real-time analysis** that presents relevant, actionable data, which is crucial for both businesses and researchers.

### **Research Question**

The central question of our research is:  
**How do search trends for different keywords compare in terms of popularity over time, and by region in Vietnam?**

This investigation will help uncover the relative public interest in each keyword, how this interest fluctuates over time, and how it is distributed across various regions of Vietnam. Understanding these dynamics can provide critical insights into the social and cultural factors influencing public interest.

---

## **Project Details**

### **Motivation**

Our primary motivation behind creating this tool is to build a practical, user-friendly dashboard that can provide immediate, meaningful insights into trending keywords. We want this tool to be accessible to individuals, even those without a technical background, to help them quickly grasp public interest in various topics. This dashboard will not only present trends over time but also break down data by subregions, offering localized insights. Additionally, it will explore correlations between keywords, making it easier to draw conclusions and identify patterns.

The following are some applications:
- **Business and Marketing**: Companies can use the tool to gauge the popularity of their products, services, or brands, and adjust their strategies accordingly.
- **Public Interest**: Researchers and policymakers can track the evolution of public interest in various topics over time, helping them better understand societal shifts.
- **Forecasting**: By analyzing historical trends, this tool can assist in forecasting the future popularity of specific keywords or topics.

This tool will be particularly beneficial for individuals who are not familiar with data analysis tools like Google Trends or advanced artificial intelligence tools (such as ChatGPT or Gemini), enabling them to easily analyze trends and gain insights without requiring any technical knowledge.

### **Project Scope and Limitations**

Due to time constraints, the scope of the project will be limited. The dashboard will support the analysis of only *three (3)* keywords at a time, and the analysis will be limited to four predefined chart types. Additionally, the length of the LLM-generated analysis will be kept concise for user readability. The dashboard will feature the following visualizations:

1. **Bar Chart**: Displays the average number of searches for each keyword over a given time period.
2. **Line Chart**: Tracks how the search interest fluctuates over time for each keyword.
3. **Interactive Map**: Visualizes the regional breakdown of search interest across different subregions of Vietnam.
4. **Scatter Plot**: Examines the relationship between the popularity of the keywords across various countries.

![Project Diagram](assets/diagram.png)

---

## **Project Plan and Timeline**

| Week | Task | Assignee |
|---|---|---|
| **1 (21/04 - 27/04)** | Brainstorm ideas and prepare a detailed proposal | Hung & Tri |
| **2 (27/04 - 04/05)** | Develop code to retrieve data from Google Trends API using R and structure it into a suitable data frame | Hung |
|  | Develop code to interact with the OpenAI API and generate keyword analysis based on prompts | Tri |
|  | Conduct peer reviews of the proposal and code | Hung & Tri |
| **3 (05/05 - 15/05)** | Develop the line chart, scatter plot, and implement filters | Hung |
|  | Build the interactive map and bar chart visualizations | Tri |
|  | Revise the proposal and refine features | Hung & Tri |
| **4 (16/05 - 30/05)** | Finalize the report and presentation; clean up code files for deployment | Hung & Tri |

---

## **Conclusion**

By developing this interactive search trend analysis dashboard, we aim to provide an easy-to-use tool that unlocks valuable insights into search trends across Vietnam. This tool will empower businesses, researchers, and the general public to track and compare the popularity of specific keywords over time, assess regional interest, and identify correlations between trends.

This project not only enhances our understanding of real-time public interest but also has practical applications across various fields, including business strategy, research, and forecasting. With the help of Google Trends and Large Language Models, we can offer a powerful analytical tool that is both accessible and informative for a wide audience.
