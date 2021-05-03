<!-- PROJECT LOGO -->
<br />
<p align="center">
  <h3 align="center">New York City Rent Explorer</h3>
  <p align="center">
    The purpose of this Shiny app is to provide insights for renters, landlords, and multifamily market researchers in New York City with up to date and useful analysis of rental market data and trends. 
    <br />
    <a href="https://github.com/STAT-413-613-21S/fp_final-project-cjy"><strong>Explore the repo »</strong></a>
  </p>
</p>


<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#data">Data</a></li>
        <li><a href="#use-case">Use Case</a></li>
        <li><a href="#limitations">Limitations</a></li>
      </ul>
    </li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgements">Acknowledgements</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

This Shiny app is the result of our final project assignment in STAT-413/613 at American University in D.C. We were interested in creating a web app that would be both useful and interesting/challenging to make. The purpose of the Shiny app is to essentially provide people in New York City, whether they be renters seeking a new apartment to live in, landlords determining what to charge renters based on their area, or multifamily market researchers attempting to understand the market more fully with informative visualizations and predictions of future rents. 

### Data

All rental data is collected from [StreetEasy.com](https://streeteasy.com/blog/download-data/), an online real estate and apartment marketplace compnay exclusively for New York City homes and apartments. The data from StreetEasy includes has the following variables: 

* Borough
* Neighborhood 
* Date (every month from January 2010 - April 2021) 
* Studio Medan Rent 
* One Bedroom Median Rent 
* Two Bedrooms Median Rent
* Three+ Bedrooms Median Rent

The rent data for each apartment type is the median rent price because the median is less affected by outliers and if a sample is highly skewed, which is common in rental data, then the median is the best measurement of central tendency than the mean. 

The boundary data for NYC to create the leaflet map come from [data.beta.nyc](https://data.beta.nyc/).

### Use Case

The purpose of this app is to help parties interested in New York City rent data. For example, you may be a renter in New York City wondering if you're paying a fair amount of rent to your landlord. This app could help you compare what you're being charged now to the usual tenant in your area. Also, you can see what your rent should likely in the next 1, 5, or more years. 

Alternatively, if you are a prospective landlord attempting to break into the rental market and figure out what you should be charging for rent in your area, you can get a general idea by using this application and can explore median rents in other area. 

In truth, there are many ways in which this app can be applied and used. If you are just interested in exploring New York City rent data at all, then this Shiny app is a great way to do so.

### Limitations 

As responsible data scientists, it is our duty to point out the limitations of this Shiny app and the data. 

This data used in the app only includes median rents from each neighborhood for each month. It is important to note that variation in rent may vary widely in some neighborhoods, which this app cannot demonstrate. Moreover, some neighborhoods have much less data than other neighborhoods. Belmont, Bronx has almost no data for studios and one bedrooms whereas Chelsea, Manhattan has plenty of data going all the way back to 2010. The app will give an appropriate warning when there are less than 10 data points, but this fact is still important to keep in mind.

The data used also excludes Staten Island completely, and does not include some neighborhoods from the other boroughs. For example, one neighborhood of importance that is not present in the data is Hell's Kitchen (also known as Clinton). 

While this applucation may be a good starting place to explore, compare, and predict rents in New York City neighborhoods, it should not serve as the only resource for these things. 

<!-- CONTRIBUTING -->
## Contributing

Now that you've read about some of our limitations, please feel free to help! Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin AmazingFeature`)
5. Open a Pull Request

<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE` for more information.

<!-- CONTACT -->
## Contact

Josh Vera O'Steen - jo7082a@student.american.edu

Yashar Mansouri - ym7190a@student.american.edu

Christopher Hoffman - ch7710a@student.american.edu

Project Link: [https://github.com/STAT-413-613-21S/fp_final-project-cjy](https://github.com/STAT-413-613-21S/fp_final-project-cjy)

<!-- ACKNOWLEDGEMENTS -->
## Acknowledgements

We'd like to thank Professor Richard Ressler for his help and guidance in completing this project (and teaching us how to do it in the first place). We'd also like to thank our coffee machines for helping keep us caffeinated while building this Shiny app. 

<br> <!-- Some space at bottom of page so acknowledgements --> 

