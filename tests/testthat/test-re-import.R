context("test-re-import.R")

# # # prepare data
# jst_import_zip("inst/extdata/pseudo_dfr.zip", import_spec = jst_define_import(article = c(jst_get_article,
#     jst_get_authors, jst_get_references, jst_get_footnotes), book = c(jst_get_book, jst_get_chapters),
#     ngram1 = jst_get_ngram), out_path = "tests/testthat/testfiles/re-import/",
#     out_file = "wi_col", col_names = T)
# 
# jst_import_zip("inst/extdata/pseudo_dfr.zip", import_spec = jst_define_import(article = c(jst_get_article,
#     jst_get_authors, jst_get_references, jst_get_footnotes), book = c(jst_get_book, jst_get_chapters),
#     ngram1 = jst_get_ngram), out_path = "tests/testthat/testfiles/re-import/",
#     out_file = "wo_col", col_names = F)
# 
# # create two batches by simply copying.
# files <- list.files("tests/testthat/testfiles/re-import", full.names = T)
# new_names <- stringr::str_replace_all(files, "-1", "-2")
# file.copy(files, new_names)
# 
# library(tidyverse)
# # # create two separate files with references and footnotes in them
# read_csv("tests/testthat/testfiles/re-import/wo_col_journal_article_jst_get_footnotes-1.csv",
#          col_names = F) %>%
#   mutate(X2 = "Footnotes") %>%
#   write_csv("tests/testthat/testfiles/re-import/wo_col_journal_article_jst_get_footnotes_wi_cont-1.csv",
#             col_names = F)
# 
# read_csv("tests/testthat/testfiles/re-import/wo_col_journal_article_jst_get_references-1.csv",
#          col_names = F) %>%
#   mutate(X2 = "References") %>%
#   write_csv("tests/testthat/testfiles/re-import/wo_col_journal_article_jst_get_references_wi_cont-1.csv",
#             col_names = F)

### jst_re-import ------
# set up correct objects
# book ----
book <- structure(list(book_id = "j.ctt24hdz7", file_name = "book-chapter-standard_book", 
    discipline = "Political Science", book_title = "The 2006 Military Takeover in Fiji", 
    book_subtitle = "A Coup to End All Coups?", pub_day = 30L, pub_month = 4L, 
    pub_year = 2009L, isbn = "9781921536502; 9781921536519", publisher_name = "ANU E Press", 
    publisher_location = "Canberra", n_pages = NA_integer_, language = "eng"), 
    row.names = c(NA, -1L), class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))

# chapter -----
chapter <- structure(list(book_id = c("j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", 
    "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", 
    "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", 
    "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", 
    "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", 
    "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", 
    "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7", 
    "j.ctt24hdz7", "j.ctt24hdz7", "j.ctt24hdz7"), file_name = c("book-chapter-standard_book", 
    "book-chapter-standard_book", "book-chapter-standard_book", "book-chapter-standard_book", 
    "book-chapter-standard_book", "book-chapter-standard_book", "book-chapter-standard_book", 
    "book-chapter-standard_book", "book-chapter-standard_book", "book-chapter-standard_book", 
    "book-chapter-standard_book", "book-chapter-standard_book", "book-chapter-standard_book", 
    "book-chapter-standard_book", "book-chapter-standard_book", "book-chapter-standard_book", 
    "book-chapter-standard_book", "book-chapter-standard_book", "book-chapter-standard_book", 
    "book-chapter-standard_book", "book-chapter-standard_book", "book-chapter-standard_book", 
    "book-chapter-standard_book", "book-chapter-standard_book", "book-chapter-standard_book", 
    "book-chapter-standard_book", "book-chapter-standard_book", "book-chapter-standard_book", 
    "book-chapter-standard_book", "book-chapter-standard_book", "book-chapter-standard_book", 
    "book-chapter-standard_book", "book-chapter-standard_book", "book-chapter-standard_book", 
    "book-chapter-standard_book", "book-chapter-standard_book"), part_id = c("j.ctt24hdz7.1", 
    "j.ctt24hdz7.2", "j.ctt24hdz7.3", "j.ctt24hdz7.4", "j.ctt24hdz7.5", "j.ctt24hdz7.6", 
    "j.ctt24hdz7.7", "j.ctt24hdz7.8", "j.ctt24hdz7.9", "j.ctt24hdz7.10", "j.ctt24hdz7.11", 
    "j.ctt24hdz7.12", "j.ctt24hdz7.13", "j.ctt24hdz7.14", "j.ctt24hdz7.15", 
    "j.ctt24hdz7.16", "j.ctt24hdz7.17", "j.ctt24hdz7.18", "j.ctt24hdz7.19", 
    "j.ctt24hdz7.20", "j.ctt24hdz7.21", "j.ctt24hdz7.22", "j.ctt24hdz7.23", 
    "j.ctt24hdz7.24", "j.ctt24hdz7.25", "j.ctt24hdz7.26", "j.ctt24hdz7.27", 
    "j.ctt24hdz7.28", "j.ctt24hdz7.29", "j.ctt24hdz7.30", "j.ctt24hdz7.31", 
    "j.ctt24hdz7.32", "j.ctt24hdz7.33", "j.ctt24hdz7.34", "j.ctt24hdz7.35", 
    "j.ctt24hdz7.36"), part_label = c(NA, NA, NA, NA, "1.", "2.", "3.", "4.", 
    "5.", "6.", "7.", "8.", "9.", "10.", "11.", "12.", "13.", "14.", "15.", 
    "16.", "17.", "18.", "19.", "20.", "21.", "22.", "23.", "24.", "25.", "26.", 
    "27.", "28.", "29.", "30.", "31.", NA), part_title = c("Front Matter", "Table of Contents", 
    "Acronyms and abbreviations", "Authors’ biographies", "The enigmas of Fiji’s good governance coup", 
    "‘Anxiety, uncertainty and fear in our land’:", "Fiji’s December 2006 coup:", 
    "‘This process of political readjustment’:", "The changing role of the Great Council of Chiefs", 
    "The Fiji military and ethno-nationalism:", "Swim or sink:", "The great roadmap charade:", 
    "Religion and politics:", "The good, the bad and the faithful:", "Heading for the scrap heap of history?", 
    "The Fiji nurses’ strike", "The Fiji coup six months on:", "State control and self-censorship in the media after the coup", 
    "The impact of the coup on Fiji’s judiciary", "The erosion of judicial independence", 
    "The rule of law and judicial independence amidst the coups and attempted coups in Fiji since 1987", 
    "The coup d’état and the Fiji Human Rights Commission", "The People’s Charter:", 
    "‘Democracy’ versus good governance", "From fear and turmoil to the possibilities of hope and renewal once again", 
    "Resolving the current crisis in Fiji – a personal perspective", "Mythic constitutionalism:", 
    "Creating a stable Fiji", "Making votes count:", "The impact of Fiji’s 2006 coup on human and women’s rights", 
    "Reflections on Fiji’s ‘coup culture’", "Fijian Ethno-Nationalism", 
    "Ethno-Nationalism and the People’s Charter", "One hand clapping:", "Fiji’s Coup Syndrome", 
    "Index"), part_subtitle = c(NA, NA, NA, NA, NA, "Fiji’s road to military coup, 2006", 
    "Who, what, where and why?", "The aftermath of the 2006 Fiji Coup", NA, 
    "Analyzing the paradox", "The post-coup economy in limbo", "Electoral issues in post-coup Fiji", 
    "The Christian churches and the 2006 coup in Fiji", "The response by Indian religious groups", 
    "The consequences of the coup for the Fiji labour movement", NA, "The role of the media", 
    NA, NA, NA, NA, NA, "For or against?", NA, NA, NA, "Whither Fiji’s course in June 2007?", 
    NA, "The need for electoral reform", NA, NA, NA, NA, "Reflections on the first anniversary of Fiji’s 2006 coup", 
    NA, NA), authors = c(NA_character_, NA_character_, NA_character_, NA_character_, 
    NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, 
    NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, 
    NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, 
    NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, 
    NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, 
    NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, 
    NA_character_, NA_character_), abstract = c(NA, NA, NA, NA, "Fiji’s December 2006 coup defied the assumptions upon which that country’s post-independence history had hitherto been written. Until then, it had been assumed that the indigenous Fijians would control the country’s politics. Even in the 1970s, 1980s and 1990s, when numbers of indigenous Fijians and Fiji Indians were close to parity, election victories by parties with predominantly Fiji Indian support had each entailed constitutional crises (April 1977) or coups (1987, 1999-2000). Writing of the 1987 election with the benefit of hindsight, one scholar described the objectives of the leftist and multiracially oriented but largely Indian-backed Fiji Labour Party – which briefly", 
    "Fiji experienced the whole gamut of emotions over the course of a fateful 2006. The year ended on an unsettled note, as it had begun. Fiji was yet again caught in a political quagmire of its own making, hobbled by manufactured tensions, refusing to heed the lessons of its recent tumultuous past, and reeling from the effects of the coup. Ironies abound. A Fijian army confronted a Fijian government, fuelling the indigenous community’s worst fears about a Fijian army spilling Fijian blood on Fijian soil. The military overthrow took place 19 years to the day after frustrated coup-maker of 1987", 
    "The Fiji military’s ‘clean-up’ coup reached a climax on 5 December 2006. Although President Ratu Josefa Iloilo rubber-stamped the takeover that morning,² he was swayed by Vice-President Ratu Joni Madraiwiwi to disassociate himself from it in the afternoon. The official statement from Government House said that the Republic of Fiji Military Forces (RFMF) had acted ‘contrary to the wishes of their Commander in Chief ’, but conveyed the President’s intention to remain in office only to preserve some semblance of continuity.³ That was not to be. Because the Prime Minister had declined to resign and the President equivocated, the illegality", 
    "‘We consider that Fiji has reached a crossroads and that the government and all those empowered to make decisions in our constitutional democracy are unable to make these decisions to save our people from destruction’, Commodore Josaia Voreqe (Frank) Bainimarama told Fiji at 6 pm on 5 December 2006. The military, which had ‘observed the concern and anguish of the deteriorating state of our beloved Fiji’, had, therefore, ‘taken over the government as executive authority in the running of the country’. Those fateful words brought to a close the long-running saga of escalating tension and the mounting war of words", 
    "The central dilemma in Fiji’s political development has been the problem of how to devise constitutional government that can reconcile the indigenous Fijian conviction of entitlement to political pre-eminence with a just representation of the interests of other sections of the population, primarily the Indians. Both the army and the Great Council of Chiefs (GCC) have gained prominence during the last 20 years as institutions of indigenous Fijian power with capacities for the extra-constitutional management of crises arising after the electoral defeat of Fijian-dominated governments. Both have grappled with the dangerous force of Fijian nationalist sentiment, at times endorsing it", 
    "Despite three years of regular public criticism and threats by the commander of the Republic of Fiji Military Forces (RFMF), Commodore Voreqe (‘Frank’) Bainimarama, most people in Fiji during the period 2002–06 thought he would not overthrow the government of Laisenia Qarase. The rank and file in the RFMF were solidly ethnic Fijian and had backed Qarase’s SDL party in large numbers. Bainimarama had put Qarase into office, and the RFMF had initially backed the latter’s ‘blueprint’ for lifting Fijian living standards. It seemed odd that Bainimarama could so change his political tune. The commander himself repeatedly said that", 
    "The interim finance minister, Mahendra Chaudhry, in launching the revised budget for 2007, stated that the economy was in a ‘sink or swim’ situation, but that he was determined to swim out of the turbulence that followed the military coup of December 2006. As of mid-2007, the state of the economy could have been described as being in limbo, and a year later things were hardly better. Fiji’s coups have been costly. Each coup has pushed back the economy some three years in terms of per capita income. More importantly, all of the four past coups have contributed to the", 
    "Electoral issues were to figure prominently in the wake of Fiji’s 2006 coup – as instruments in the ideological justification of the military takeover; as internationally required stepping stones on the ‘roadmap’ back to democracy; and as predominant features of the interim government’s vision for social transformation. The coup-makers and their supporters justified the seizure of power on the grounds that the May 2006 election had been marred by ethnically biased ballot-rigging; that the preparations of the Elections Office had been gravely mismanaged; that the constitution had been violated; and that Qarase’s Soqosoqo Duavata ni Lewenivanua (SDL) party had abused its", 
    "On 5 December 2006, Commodore Bainimarama led a successful military coup against a Qarase government that had been strongly supported by the Assembly of Christian Churches of Fiji (ACCF), an umbrella organization in which the Methodist Church of Fiji is the most dominant member. Within two days of the coup, the Assistant General Secretary of the Methodist Church, in his capacity as chair of both the ACCF and a second umbrella group, the Fiji Council of Churches (FCC), condemned the coup as illegal and unconstitutional. This position has been retained – by the Methodist Church in particular, despite its support for", 
    "Shortly after lunchtime on 5 December 2006, crowds began to assemble outside Prime Minister Laisenia Qarase’s residence on Richards Road, Suva. At first they arrived slowly, a few at a time; but, as the afternoon progressed, numbers rapidly swelled, particularly after the arrival of the Republic of Fiji Military Forces (RFMF).² The crowds were drawn by the news that their government was in the process of being overthrown, and the rumour that their prime minister would shortly be placed under house arrest.³ The congregated sang mournful hymns and offered prayers for the nation in both Fijian and English. They rallied", 
    "During the period 2001–06, in between his repeated media skirmishes with Prime Minister Laisenia Qarase’s government, the commander of the Republic of Fiji Military Forces (RFMF), Commodore Frank Bainimarama, proclaimed his intention to depose Qarase’s Soqosoqo Duavata ni Lewenivanua (SDL) party from government.¹ Even though Qarase had, for the first time, followed Fiji constitution’s power-sharing provision, and established a multiparty government, with ministerial appointments for eight Fiji Labour Party (FLP) members of parliament, it was evident, from three very controversial bills, that Qarase continued to push an ethno-nationalist agenda; this was used by the Commodore as a justification for", 
    "On 5 December 2006, when news of the 2006 coup surfaced, I was with seven senior members of the Fiji Nursing Association (FNA) at our MacGregor Road headquarters. We were practicing presentations of the FNA’s submission on the draft Radium Protection Bill. A special cabinet select committee was scheduled to meet with us at 10am that morning. Just as we left the office to go to Parliament House, mobile phones started ringing as distressed family members called their relatives about the coup and the trouble at the parliamentary complex at Veiuto. Amongst the callers was my secretary, calling to alert", 
    "I came across a very interesting story not too long ago, the headline-grabbing type, one sure to be a best seller. The story revealed the wisdom and farsightedness of the founders of modern Fiji as portrayed in the islands’ coat of arms. The sugar cane on the national emblem is reflective of the leadership of Fiji’s first modern leader and prime minister, the late Ratu Sir Kamisese Mara, and his unquestioned role in securing unparalleled prices for our sugar in Europe. Then there’s the coconut palm, symbolic – the story claimed – of the policies and leadership of the late Dr Timoci", 
    "Fiji’s media grew exponentially in the three decades following independence. With that growth and diversification came a radically altered relationship between the media and the government.At independence the media consisted of The Fiji Times, then a reliably pro-establishment organ, and the government radio station, which, while nominally independent, could be relied upon not to rock the boat too much. By 2006, Fiji had three national dailies, a range of magazines, a flourishing independent radio industry and a monopoly television broadcaster. Per head of population Fiji had and continues to have among the greatest choice of local media in the", 
    "The December 2006 coup had important ramifications for Fiji’s judiciary.¹ It was Fiji’s fourth coup in a little under 20 years. What was significant about this one was the extent to which senior members of the Bench and Bar appear to have been complicit, either before the event or subsequently. There is as yet an incomplete awareness in the wider community about the sanctity of the rule of law. In circumstances such as a coup, where the ordinary person looks to lawyers for leadership and guidance and finds instead ambivalence and dissembling, the implications, both immediate and long term, are", 
    "Fifteen months after the coup, Fiji is drifting inexorably towards the abyss. As every day goes by, new examples of the capricious and unprincipled assertion of power without authority emerge. The new status quo is the product of an equilibrium of fear – on the one hand, the people fear the instruments of power and being pressed into silence and, on the other hand, those who wield power in Fiji fear the light of any objective scrutiny and debate. As a result, the authorities have become habituated to the use of coercion and intimidation to control the free circulation of ideas", 
    "The purported ‘coup’ or rebellion¹ of 5 December 2006 had a major impact on the Fiji judiciary, and the rule of law. But it did not come out of the blue. The judicial schisms that were exposed in early 2007 followed a pattern established much earlier, in the wake of the 1987 and 2000 coups. This chapter examines those earlier events, showing how they shaped the legal fraternity in Fiji and set the stage for the late 2006 crisis.When Lieutenant Colonel Sitiveni Ligamamada Rabuka and the Fiji Military Forces entered the parliament of Fiji with arms on 14 May", 
    "The report of the Fiji Human Rights Commission (‘the Commission’), ‘The Assumption of Executive Authority on December 5th 2006 by Commodore J.V. Bainimarama, Commander of the Republic of Fiji Military Forces: Legal, Constitutional and Human Rights Issues’, is a remarkable apology for the military regime in Fiji. There are two elements in it that lead to this conclusion. The first (which has two parts) is the assertion that:The RFMF overthrew an illegally constituted, unconstitutional Government which was acting against the public interest in violation of public security and public safety protections in the Constitution.¹This amounts not only to", 
    "The military junta is in the process of creating a ‘People's Charter’ for Fiji. The charter's objectives (ending racism and corruption and ensuring good governance) are praiseworthy. And the charter has the support of eminent persons, like Archbishop Petero Mataca. But the eminent persons forget the hard lesson of the 2000 crisis: That ‘the method is just as important as the cause’ and that how the charter is put in place is just as important as the good objectives themselves. The public are being asked to build a castle on a foundation of sand, violently thrown up on the beach", 
    "Fiji’s intelligentsia – be they academics, politicians, clerics, the legal fraternity or the suddenly vocal human rights activists – have been deeply divided by the events of 5 December 2006 and the best way forward for the nation.From day one, a marked delineation in thinking has been obvious between those who see the takeover of government by army commander Commodore Voreqe Bainimarama as timely, the only escape route for Fiji from the imminent national disaster posed by six years of misrule by Laisenia Qarase’s Soqosoqo Duavata ni Lewenivanua (SDL) government; and others who champion a return to the Laisenia Qarase-style ‘democracy’", 
    "The Republic of Fiji Military Forces (RFMF), led by Commodore Frank Bainimarama, plunged Fiji into yet another great crisis when it used force of arms to remove the SDL/Labour multiparty government on 5 December 2006. This act of treason had been building for some time. The Commodore had created fear and anxiety in the minds of the people through a series of threats to stage a coup.When he finally acted, Commodore Bainimarama brutally knocked Fiji off the course it was then following for a secure and prosperous future, founded on a new political accommodation between the indigenous community and", 
    "Public reaction to the military coup of 5 December 2006 has taken several forms. First, there has been a reaction from those who were directly and personally affected; the victims of the coup and the ‘clean-up’ campaign. These included members of the ousted Qarase government; high officials in the civil service, statutory bodies, public enterprises and other organizations who have lost their jobs; and members of the suspended Great Council of Chiefs (GCC). For them, the focus of attention is the legality of the coup and other subsequent actions.The second group includes those who have not been personal victims", 
    "Reflecting on the statement made by the commander of the Republic of Fiji Military Forces (RFMF), Commodore J. V. Bainimarama, upon seizing power and ousting the multiparty government of Prime Minister Laisenia Qarase on 5 December 2006, one is bemused by some of the commander’s rhetoric. In his remarks he stated inter alia:… The RFMF could have carried out unconstitutional and illegal activities, but had not done so and will not do so. It believes in the rule of law and shall adhere to the Constitution. It not only adheres to the rule of law and Constitution, but more", 
    "Two years ago, Fiji was on the threshold of having a multiparty cabinet. On that occasion, I used a Fijian saying, ‘kunekunea na eloka ni dilio’, to describe the rare opportunity it gave us to start afresh. The phrase is a reference to the rarity of finding the eggs of a particular bird. This is so because the bird in question actually lays its eggs in Alaska before returning to Fiji to live. We were unable to take full advantage of the moment and have since regressed. So what is it that can be done at this point to create", 
    "Fiji will be at the crossroads again in 2009. In a span of 10 years, since the first election under the 1997 constitution was contested in 1999, the country will have had four elections. The 1997 constitution – which ordains the alternative vote (AV) system, the use of a mix of open and communal seats, and the multiparty cabinet concept – had been expected to encourage moderation and accommodation, resulting in a more representative and multi-ethnic cabinet. This, sadly, has not happened. On the contrary, the racial groups have become more polarized, and political parties have played the race card to the", 
    "In December 2006, the commander of the Republic of Fiji Military Forces (RFMF), Commodore Voreqe Bainimarama, overthrew the Laisenia Qarase-led multiparty government by claiming to invoke the ‘doctrine of necessity’. He defended his actions by citing legal precedent that supported his ‘clean-up campaign’ to eliminate corruption and racism in government.Self-appointed President Bainimarama declared a state of emergency in Fiji, arguing that this was the only way that the army could fulfill the aims of its campaign. According to the military, certain fundamental rights and freedoms were suspended as a result of the state of emergency. It warned the public", 
    "Discussions about Fiji’s politics inevitably revolve around military coups. For many years Fiji was preoccupied with the turbulence and aftermath of the two coups in 1987. Then it was the 2000 coup that preoccupied people. Now we have another coup to analyze, to explore, to use as our current reference point; one which took place in December 2006. This installed a military-backed and led interim regime, with the purported mandate of the President.The periodic upheavals we have experienced over the last twenty years have given rise to the perception that we have a ‘coup culture’. Whether this ‘coup culture’", 
    "According to Jerry Muller, there are two major ways of thinking about national identity.² One is that all people who live within a country's borders are part of the nation, regardless of their ethnic, racial, or religious origins. This liberal or civic nationalist interpretation is the view taken by those drafting Fiji’s People’s Charter. But the liberal view has competed with – and often lost out to – a different view, that of ethno-nationalism. The core of the ethno-nationalist idea is that nations are defined by a shared heritage, which usually includes a common language, a common faith, and a common ethnic", 
    "It has been suggested that it is difficult to promote a common national identity – a national moral vision, narrative, rituals and symbols – because:… national identities come ‘naturally’ where people are defined by a discretely bordered territory, a stable and sovereign political state, a common language, a common culture and a common history. A national consciousness is easiest when ethnic or race is singular rather then plural (such as in a multiracial, multicultural and multireligious country such as Fiji).²However, in the modern world, there is hardly any country that fits such a narrow classical definition. Almost every country today", 
    "The first anniversary of Fiji’s December 2006 coup passed uneventfully, without any rallies, protest marches or vigils – merely an exhausted, scarcely audible mutter from the populace hankering for some semblance of normality. A Fijian political activist once likened Fiji to a swimming duck: All calm on the surface but furiously churning underneath. Just how much turbulence there was among indigenous Fijians is difficult to gauge, but its existence was beyond doubt. To forestall any organized opposition, the interim administration slapped on several states of emergency.Overall, 2007 remained a depressing and miasmic year, with much movement but little change. The", 
    "Fiji’s 2006 coup was, in some ways, curious by international standards. It was not driven by poverty or economic backwardness, although that is often a ‘common denominator’ amongst coups.¹ Samuel Huntington’s claim that successful coups are confined to countries with income per capita at less than US$500 scarcely fits the experience of Fiji, where average income is more than four times that.² Fiji would also sit awkwardly alongside Samuel Finer’s crude continuum between ‘less developed’ states, which, he states, are particularly vulnerable to military coups, and ‘more mature’ states, which are not.³ Fiji is the most economically developed of its", 
    NA), part_first_page = c("i", "v", "vii", "xi", "3", "21", "43", "67", "97", 
    "117", "139", "155", "187", "209", "237", "253", "267", "277", "291", "301", 
    "311", "321", "339", "343", "353", "375", "385", "393", "397", "405", "409", 
    "415", "419", "425", "449", "459")), row.names = c(NA, -36L), class = c("spec_tbl_df", "tbl_df", 
    "tbl", "data.frame"))
# article -----
article <- structure(list(file_name = "journal-article-standard_case", journal_doi = NA_character_, 
    journal_jcode = "kewbulletin", journal_pub_id = NA_character_, journal_title = "Kew Bulletin", 
    article_doi = "10.2307/4117222", article_pub_id = NA_character_, article_jcode = NA_character_, 
    article_type = "research-article", article_title = "Two New Species of Ischaemum", 
    volume = "5", issue = "2", language = "eng", pub_day = "1", pub_month = "1", 
    pub_year = 1950L, first_page = "187", last_page = "188", page_range = "187-188"), 
    row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame"))
# authors -----
authors <- structure(list(file_name = "journal-article-standard_case", prefix = NA_character_, 
    given_name = "N. L.", surname = "Bor", string_name = NA_character_, suffix = NA_character_, 
    author_number = 1L), row.names = c(NA, -1L), class = c("tbl_df", "tbl", 
    "data.frame"))
# footnotes ----
footnotes <- structure(list(file_name = "journal-article-standard_case", footnotes = NA_character_), 
    row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame"))
footnotes_wi_cont <- structure(list(file_name = "journal-article-standard_case", footnotes = "Footnotes"), 
                       row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame"))
# references ----
references <- structure(
  list(
    file_name = "journal-article-standard_case", 
    ref_title = NA_character_,
    ref_authors = NA_character_,
    ref_editors = NA_character_,
    ref_collab = NA_character_,
    ref_item_title = NA_character_,
    ref_year = NA_character_,
    ref_source = NA_character_,
    ref_volume = NA_character_,
    ref_first_page = NA_character_,
    ref_last_page = NA_character_,
    ref_publisher = NA_character_,
    ref_publication_type = NA_character_,
    ref_unparsed = NA_character_), row.names = c(NA, -1L), class = c("tbl_df", 
    "tbl", "data.frame"))

references_wi_cont <- structure(
  list(
    file_name = "journal-article-standard_case", 
    ref_title = "References",
    ref_authors = NA_character_,
    ref_editors = NA_character_,
    ref_collab = NA_character_,
    ref_item_title = NA_character_,
    ref_year = NA_character_,
    ref_source = NA_character_,
    ref_volume = NA_character_,
    ref_first_page = NA_character_,
    ref_last_page = NA_character_,
    ref_publisher = NA_character_,
    ref_publication_type = NA_character_,
    ref_unparsed = NA_character_), 
  row.names = c(NA, -1L), 
  class = c("tbl_df", "tbl", "data.frame"))

# ngrams -----
ngram <- structure(list(file_name = c("book-chapter-standard_book", "book-chapter-standard_book"), 
    ngram = c("Common", "Uncommon"), n = c(400L, 5L)), row.names = c(NA, -2L), 
    class = c("tbl_df", "tbl", "data.frame"))


# tests ----
test_that("files with column names can be re-read", {
    expect_equal(
      jst_re_import("testfiles/re-import/wi_col_book_chapter_jst_get_book-1.csv"), 
      book
    )
    expect_equal(
      jst_re_import("testfiles/re-import/wi_col_book_chapter_jst_get_chapters-1.csv"), 
      chapter
    )
    expect_equal(
      jst_re_import("testfiles/re-import/wi_col_journal_article_jst_get_article-1.csv"), 
      article
    )
    expect_equal(
      jst_re_import("testfiles/re-import/wi_col_journal_article_jst_get_authors-1.csv"), 
      authors
    )
    expect_equal(
      jst_re_import("testfiles/re-import/wi_col_journal_article_jst_get_footnotes-1.csv"), 
      footnotes
    )
    expect_equal(
      jst_re_import("testfiles/re-import/wi_col_journal_article_jst_get_references-1.csv"), 
      references
    )
    expect_equal(
      jst_re_import("testfiles/re-import/wi_col_ngram1_jst_get_ngram-1.csv"), 
      ngram
    )
})


test_that("files without column names can be re-read", {
    expect_equal(
      jst_re_import("testfiles/re-import/wo_col_book_chapter_jst_get_book-1.csv"), 
      book
    )
    expect_equal(
      jst_re_import("testfiles/re-import/wo_col_book_chapter_jst_get_chapters-1.csv"), 
      chapter
    )
    expect_equal(
      jst_re_import("testfiles/re-import/wo_col_journal_article_jst_get_article-1.csv"), 
      article
    )
    expect_equal(
      jst_re_import("testfiles/re-import/wo_col_journal_article_jst_get_authors-1.csv"), 
      authors
    )
    expect_equal(
      jst_re_import("testfiles/re-import/wo_col_ngram1_jst_get_ngram-1.csv"), 
      ngram
    )
    expect_equal(
      jst_re_import("testfiles/re-import/wo_col_journal_article_jst_get_footnotes_wi_cont-1.csv"),
      footnotes_wi_cont
    )
    expect_equal(
      jst_re_import("testfiles/re-import/wo_col_journal_article_jst_get_references_wi_cont-1.csv"),
      references_wi_cont
    )
    expect_equal(
      jst_re_import("testfiles/re-import/wo_col_journal_article_jst_get_references-1.csv"),
      references
    )
    
})

test_that("warnings are emitted, if no file is recognized", {
  expect_warning(
    jst_re_import("testfiles/re-import/wo_col_journal_article_jst_get_footnotes-1.csv"), 
    "Unable to distinguish"
  )
  
  footnotes_unrecognized <- suppressWarnings(
    jst_re_import("testfiles/re-import/wo_col_journal_article_jst_get_footnotes-1.csv")
  )
  
  expect_named(footnotes_unrecognized, c("X1", "X2"))
})


# # jst_combine_outputs
# only test this for two parts, since the above tests cover all types
combined_book <- book %>% dplyr::slice(c(1, 1))
combined_chapter <- chapter %>% dplyr::slice(rep(1:n(), times = 2))

test_that("files with column names can be combined", {
  combined <- jst_combine_outputs("testfiles/re-import", write_to_file = FALSE,
                      warn = FALSE)

  expect_identical(purrr::pluck(combined, 1), combined_book)
  expect_identical(purrr::pluck(combined, 2), combined_chapter)
})

test_that("files can be written to dir", {
  
  # check for warning if overwriting
  
})

test_that("files are removed", {
  
})


test_that("only .csv files can be reimported", {
  expect_error(jst_re_import(jst_example("book.xml")), 
               "Only .csv-files which were generated")
})

test_that("reimporting checks inputs", {
  expect_error(
    jst_combine_outputs(list.files("testfiles/re-import/", 
                             full.names = T),
                  out_path = NULL),
    "You must specify")
  
  expect_error(jst_combine_outputs("abc"), "'abc' does not exist in current")

  
  new_dir <- tempdir()
  dir.create(file.path(new_dir, "obiwan"))
  expect_error(jst_combine_outputs(file.path(new_dir, "obiwan")), 
               "There are no files")
})


