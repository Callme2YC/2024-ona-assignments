    linkedin_data <- read.csv("/Users/yuyichen/Desktop/Winter 2024/ORGB - 672/2024-ona-assignments/Connections_Yichen.csv")

    # 1. Count of contacts by current employer
    employer_counts <- linkedin_data %>%
      group_by(Company) %>%
      summarise(Count = n())

    # Print the counts
    print(employer_counts)

    ## # A tibble: 160 × 2
    ##    Company            Count
    ##    <chr>              <int>
    ##  1 ""                     6
    ##  2 ".online business"     1
    ##  3 "5Y Capital"           1
    ##  4 "ADP"                  1
    ##  5 "ALDO Group"           2
    ##  6 "ALTEN"                1
    ##  7 "Acosta"               1
    ##  8 "Air Transat"          3
    ##  9 "Al Baladi Group"      1
    ## 10 "Allstate Canada"      1
    ## # ℹ 150 more rows

    # 2. Total count of contacts
    total_contacts <- nrow(linkedin_data)
    print(paste("Total contacts:", total_contacts))

    ## [1] "Total contacts: 226"

    # Ensure there is an 'id' column in linkedin_data which serves as a unique identifier
    linkedin_data <- linkedin_data %>%
      mutate(id = row_number())

    # Creating a new 'label' column with first name and the first letter of last name
    linkedin_data <- linkedin_data %>%
      mutate(label = paste(First.Name, substr(Last.Name, 1, 1)))

    # Create nodes dataframe
    nodes <- linkedin_data %>%
      select(id, label)

    # Creating edges DataFrame
    # Ensure that we are only considering companies with more than one person
    edges <- linkedin_data %>%
      group_by(Company) %>%
      filter(n() > 1) %>%
      do({
        ids <- .$id
        tibble(from = combn(ids, 2)[1, ], to = combn(ids, 2)[2, ])
      }) %>%
      ungroup() %>%
      select(from, to)

    # Get the count of contacts by their current employer
    count_by_company <- linkedin_data %>%
      count(Company)

    # Total count of contacts
    total_contacts <- nrow(linkedin_data)

    # Output the counts
    print(count_by_company)

    ##                                                                         Company
    ## 1                                                                              
    ## 2                                                              .online business
    ## 3                                                                    5Y Capital
    ## 4                                                                           ADP
    ## 5                                                                    ALDO Group
    ## 6                                                                         ALTEN
    ## 7                                                                        Acosta
    ## 8                                                                   Air Transat
    ## 9                                                               Al Baladi Group
    ## 10                                                              Allstate Canada
    ## 11                                                  America Business Management
    ## 12                                           Ameriprise Financial Services, LLC
    ## 13                                                                 AtkinsRéalis
    ## 14                                                  Atlantic Equipment Services
    ## 15                                                             Axioms Analytics
    ## 16                                                  BC NEUROIMMUNOLOGY LABS INC
    ## 17                                                                          BDC
    ## 18                                                                  BNP Paribas
    ## 19                                                                   BOMBARDIER
    ## 20                                                                          BRP
    ## 21                                                  Battaglia Advisory Services
    ## 22                                                         Boehringer Ingelheim
    ## 23                                                              Boldyn Networks
    ## 24                                                                          CAE
    ## 25                                                                          CGI
    ## 26                                                                           CN
    ## 27                                                       Caitong Securities Ltd
    ## 28                                                  Canada Post / Postes Canada
    ## 29                           Canada Revenue Agency - Agence du revenu du Canada
    ## 30                                                                        CapFi
    ## 31                                                                    Capgemini
    ## 32                                    Capitale Hélicoptère - École de pilotage 
    ## 33                                               Ciatek IT & software solutions
    ## 34                                                                        Coach
    ## 35                                                            Compass Analytics
    ## 36                   Correctional Service Canada │ Service correctionnel Canada
    ## 37                                    DINKAR FINANCIAL SERVICES PRIVATE LIMITED
    ## 38                                                         Dalhousie University
    ## 39                                                 Desautels Capital Management
    ## 40                                                        Design Group Staffing
    ## 41                                                                     DoorDash
    ## 42                                                         DoubleTree by Hilton
    ## 43                                                         Dress me collection 
    ## 44                                                                           EY
    ## 45                                                                     Estartup
    ## 46                                                              FNCEC Financial
    ## 47                                            First Onsite Property Restoration
    ## 48                                                                       Fiverr
    ## 49                                                          Foresters Financial
    ## 50                                                          Futurism Enterprise
    ## 51                                                                   GDG Lahore
    ## 52                                                                GF Securities
    ## 53                                                                    GFX Prime
    ## 54                                                    Government of Nova Scotia
    ## 55                                                  Grant Thornton LLP (Canada)
    ## 56                                                Grey Cardinal Management Inc.
    ## 57                                                    H2O ASIA CAPITAL PTE. LTD
    ## 58                                                                      HashNut
    ## 59                                                  Healthcare Systems R&A Inc.
    ## 60                                                              HeiD Enterprise
    ## 61                                                                          IDC
    ## 62                                                                   IVADO LABS
    ## 63      Industrial Technology Research Institute (ITRI)(工業技術研究院, 工研院)
    ## 64                                                                      Infosys
    ## 65                                                                  J.P. Morgan
    ## 66                                                  Jarislowsky, Fraser Limited
    ## 67                                                      John Global Enterprise 
    ## 68                                                        KPI Digital Solutions
    ## 69                                                                  KPMG Canada
    ## 70                                                                      L'Oréal
    ## 71                                     LRDG Language Research Development Group
    ## 72                                                                     LemonBox
    ## 73                                                                       Lenovo
    ## 74                                                 Lifesaver Books and Seminars
    ## 75                                                        MHI RJ Aviation Group
    ## 76                                                                         MUFG
    ## 77                                                       MUFG Investor Services
    ## 78                                                              Marine Thinking
    ## 79                                                         Marsh Canada Limited
    ## 80                                                                      MassPay
    ## 81                                                            McGill University
    ## 82                          McGill University - Desautels Faculty of Management
    ## 83                                                              McKesson Canada
    ## 84                             Medexus Pharmaceuticals (TSX: MDP, OTCQX: MEDXF)
    ## 85                                               Mensana Change Management Ltd.
    ## 86                                                                 Midas Safety
    ## 87                                                               Mile Solutions
    ## 88                                                Molson Coors Beverage Company
    ## 89                                                               Monster Energy
    ## 90                                                            Nallie Enterprise
    ## 91                                                      National Bank Financial
    ## 92     National Research Council Canada / Conseil national de recherches Canada
    ## 93                                                                       Nautel
    ## 94  New Silk Road Association of Germany（Neue Seidenstrasse  e. V. Deutschland
    ## 95                                                                 NikahForever
    ## 96                                                        Northbridge Insurance
    ## 97                                                            Nova Scotia Power
    ## 98          Ontario Government Ministry of Public and Business Service Delivery
    ## 99                                                              PSP Investments
    ## 100                                                                     Pandora
    ## 101                                                                   Pomellato
    ## 102                                                             Pratt & Whitney
    ## 103                                                      Pratt & Whitney Canada
    ## 104                                                            Procter & Gamble
    ## 105                                    Promising Maritime Training Company LTD.
    ## 106                                     Property Valuation Services Corporation
    ## 107                                                                         PwC
    ## 108                                                                    QRA Corp
    ## 109                                                 Queen's School of Computing
    ## 110                                                            Quinte Dimension
    ## 111                                                                         RBC
    ## 112                                                         RBC Capital Markets
    ## 113                                                       RBC Investor Services
    ## 114                          RI-MUHC | Research Institute of the MUHC | #rimuhc
    ## 115                                                       Royal LePage Atlantic
    ## 116                                                      SHRIGLEY BATTRICK CPAs
    ## 117                                                                         SLB
    ## 118                                                           SavvyPro Edu Inc.
    ## 119                                                              Scotlynn Group
    ## 120                                                               Self-Employed
    ## 121                                                               Self-employed
    ## 122                                                                   ShyftLabs
    ## 123                                                                Sia Partners
    ## 124                                                        Siddiq Leather Works
    ## 125                                                                Solactive AG
    ## 126                                             Strategic Arts Management (SAM)
    ## 127                                                                    Sun Life
    ## 128                                                                  Sunnybrook
    ## 129                                                                Sunwire Inc.
    ## 130                                                                   Synechron
    ## 131                                                                          TD
    ## 132                                                               TD Securities
    ## 133                                                           Techcombank (TCB)
    ## 134                                                             Tesser Insights
    ## 135                                                           The Data Mangrove
    ## 136                                               The Great Canadian Woman Inc.
    ## 137                                                       The Pint Public House
    ## 138                                            Tibah Airports Operation Co. LTD
    ## 139                                                       True North Accounting
    ## 140                                                         University of Essex
    ## 141                             University of Warwick - Warwick Business School
    ## 142                                                                  VNTodayInc
    ## 143                                              Veiva Scientific India Pvt Ltd
    ## 144                                                                  VidCruiter
    ## 145                                                                 ViraProcess
    ## 146                                                                  VirtualDNA
    ## 147                                                            Volkswagen Group
    ## 148                                                         WFG Securities Inc.
    ## 149                                           Waha Firdos Contracting LLC Dubai
    ## 150                                                 http://www.cggthinktank.com
    ## 151                                                               iGan Partners
    ## 152                                             taiping life insurance co., LTD
    ## 153                                                            École du Barreau
    ## 154                                                                    复旦大学
    ## 155                                                                    字节跳动
    ## 156                                                            工商银行珠海分行
    ## 157                                                    杉数科技（北京）有限公司
    ## 158                                                                  毕马威中国
    ## 159                                                                洲际酒店集团
    ## 160                                                                        网易
    ##      n
    ## 1    6
    ## 2    1
    ## 3    1
    ## 4    1
    ## 5    2
    ## 6    1
    ## 7    1
    ## 8    3
    ## 9    1
    ## 10   1
    ## 11   1
    ## 12   1
    ## 13   3
    ## 14   1
    ## 15   1
    ## 16   1
    ## 17   3
    ## 18   3
    ## 19   2
    ## 20   2
    ## 21   1
    ## 22   1
    ## 23   1
    ## 24   2
    ## 25   1
    ## 26   5
    ## 27   1
    ## 28   1
    ## 29   1
    ## 30   1
    ## 31   1
    ## 32   1
    ## 33   1
    ## 34   1
    ## 35   2
    ## 36   1
    ## 37   1
    ## 38   2
    ## 39   3
    ## 40   1
    ## 41   1
    ## 42   1
    ## 43   1
    ## 44   4
    ## 45   1
    ## 46   1
    ## 47   1
    ## 48   1
    ## 49   2
    ## 50   1
    ## 51   1
    ## 52   1
    ## 53   1
    ## 54   1
    ## 55   1
    ## 56   1
    ## 57   1
    ## 58   1
    ## 59   1
    ## 60   1
    ## 61   1
    ## 62   1
    ## 63   1
    ## 64   1
    ## 65   1
    ## 66   1
    ## 67   1
    ## 68   3
    ## 69   1
    ## 70   4
    ## 71   1
    ## 72   1
    ## 73   1
    ## 74   1
    ## 75   1
    ## 76   4
    ## 77   6
    ## 78   1
    ## 79   1
    ## 80   1
    ## 81  10
    ## 82   9
    ## 83   2
    ## 84   1
    ## 85   1
    ## 86   1
    ## 87   1
    ## 88   1
    ## 89   1
    ## 90   1
    ## 91   1
    ## 92   1
    ## 93   1
    ## 94   1
    ## 95   1
    ## 96   1
    ## 97   1
    ## 98   1
    ## 99   1
    ## 100  1
    ## 101  1
    ## 102  1
    ## 103  3
    ## 104  1
    ## 105  1
    ## 106  1
    ## 107  1
    ## 108  1
    ## 109  1
    ## 110  1
    ## 111  1
    ## 112  1
    ## 113  1
    ## 114  1
    ## 115  1
    ## 116  1
    ## 117  1
    ## 118  1
    ## 119  1
    ## 120  1
    ## 121  2
    ## 122  1
    ## 123  1
    ## 124  1
    ## 125  1
    ## 126  1
    ## 127  1
    ## 128  1
    ## 129  1
    ## 130  1
    ## 131  2
    ## 132  2
    ## 133  1
    ## 134  1
    ## 135  2
    ## 136  1
    ## 137  1
    ## 138  1
    ## 139  1
    ## 140  1
    ## 141  1
    ## 142  1
    ## 143  1
    ## 144  1
    ## 145  1
    ## 146  1
    ## 147  1
    ## 148  1
    ## 149  1
    ## 150  1
    ## 151  1
    ## 152  1
    ## 153  1
    ## 154  1
    ## 155  1
    ## 156  1
    ## 157  1
    ## 158  1
    ## 159  1
    ## 160  1

    print(total_contacts)

    ## [1] 226

    # Create a tidygraph object
    network <- tbl_graph(nodes = nodes, edges = edges)

    # Convert to igraph object
    g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

    # Plot the network using ggraph
    ggraph(g, layout = 'fr') +
      geom_edge_link() +
      geom_node_point(color = 'blue', size = 5) +
      geom_node_text(aes(label = label), repel = TRUE, 
                     point.padding = unit(0.2, "lines"),
                     fontface = "plain", family = "") + # Ensure default font settings
      theme_graph() +
      ggtitle("LinkedIn Network Graph")

    ## Warning: ggrepel: 73 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](Exercise-01_files/figure-markdown_strict/unnamed-chunk-10-1.png)
