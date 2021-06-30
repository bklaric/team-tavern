module TeamTavern.Client.Pages.Privacy (privacyPolicy) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Anchor (textAnchor_)
import TeamTavern.Client.Snippets.Class as HS

privacyPolicy :: forall slots action. HH.HTML slots action
privacyPolicy =
  HH.div [ HS.class_ "privacy-policy" ]
  [ HH.p_ [ HH.text "Privacy Policy" ]
  , HH.p_ [ HH.text "1. Introduction" ]
  , HH.p_ [ HH.text "1.1 We are committed to safeguarding the privacy of our website visitors and service users." ]
  , HH.p_ [ HH.text "1.2 This policy applies where we are acting as a data controller with respect to the personal data of our website visitors and service users; in other words, where we determine the purposes and means of the processing of that personal data." ]
  , HH.p_ [ HH.text "1.3 We use cookies on our website. Insofar as those cookies are not strictly necessary for the provision of our website and services, we will ask you to consent to our use of cookies when you first visit our website." ]
  , HH.p_ [ HH.text "1.4 This website is owned and operated by Branimir Klarić." ]
  , HH.p_ [ HH.text "1.5 You can contact us by email, using this email address admin@teamtavern.net" ]
  , HH.p_ [ HH.text "2. Credit" ]
  , HH.p_ [ HH.text "2.1 This document was created using a template from Docular (", textAnchor_ "https://docular.net" ")." ]
  , HH.p_ [ HH.text "3. How we use your personal data" ]
  , HH.p_ [ HH.text "3.1 In this Section 3 we have set out:" ]
  , HH.p_ [ HH.text "(a) the general categories of personal data that we may process;" ]
  , HH.p_ [ HH.text "(b) in the case of personal data that we did not obtain directly from you, the source and specific categories of that data;" ]
  , HH.p_ [ HH.text "(c) the purposes for which we may process personal data; and" ]
  , HH.p_ [ HH.text "(d) the legal bases of the processing." ]
  , HH.p_ [ HH.text "3.2 We may process data about your use of our website and services (\"usage data\"). The usage data may include your IP address, geographical location, browser type and version, operating system, referral source, length of visit, page views and website navigation paths, as well as information about the timing, frequency and pattern of your service use. The source of the usage data is our analytics tracking system. This usage data may be processed for the purposes of analysing the use of the website and services. The legal basis for this processing is consent or our legitimate interests, namely monitoring and improving our website and services." ]
  , HH.p_ [ HH.text "3.3 We may process your account data (\"account data\"). The account data may include your name and email address. The source of the account data is you, through TeamTavern registration form, but only with your explicit permission. The account data may be processed for the purposes of operating our website, providing our services, ensuring the security of our website and services, maintaining back-ups of our databases and communicating with you. The legal basis for this processing is our legitimate interests, namely the proper administration of our website and business." ]
  , HH.p_ [ HH.text "3.4 We may process your information included in your personal profile on our website (\"profile data\"). The profile data may include your nickname, email address, date of birth, location, spoken languages, microphone usage, time available online and various game specific details (e.g. rank, preferred roles and interests). The profile data may be processed for the purposes of enabling and monitoring your use of our website and services. The legal basis for this processing is our legitimate interests, namely the proper administration of our website and business." ]
  , HH.p_ [ HH.text "3.5 We may process your personal data that is provided in the course of the use of our services (\"service data\"). The service data may include the timing, frequency and pattern of service use. The source of the service data is you. The service data may be processed for the purposes of operating our website, providing our services, ensuring the security of our website and services, maintaining back-ups of our databases and communicating with you. The legal basis for this processing is our legitimate interests, namely the proper administration of our website and business." ]
  , HH.p_ [ HH.text "3.6 We may process information that you post for publication on our website or through our services (\"publication data\"). The publication data may be processed for the purposes of enabling such publication and administering our website and services. The legal basis for this processing is our legitimate interests, namely the proper administration of our website and business." ]
  , HH.p_ [ HH.text "3.7 We may process information that you provide to us for the purpose of subscribing to our email notifications and/or newsletters (\"notification data\"). The notification data may be processed for the purposes of sending you the relevant notifications and/or newsletters. The legal basis for this processing is the performance of a contract between you and us and/or taking steps, at your request, to enter into such a contract." ]
  , HH.p_ [ HH.text "3.8 We may process information contained in or relating to any communication that you send to us (\"correspondence data\"). The correspondence data may include the communication content and metadata associated with the communication. Our website will generate the metadata associated with communications made using the website contact forms. The correspondence data may be processed for the purposes of communicating with you and record-keeping. The legal basis for this processing is our legitimate interests, namely the proper administration of our website and business and communications with users." ]
  , HH.p_ [ HH.text "3.9 In addition to the specific purposes for which we may process your personal data set out in this Section 3, we may also process any of your personal data where such processing is necessary for compliance with a legal obligation to which we are subject, or in order to protect your vital interests or the vital interests of another natural person." ]
  , HH.p_ [ HH.text "3.10 Please do not supply any other person's personal data to us, unless we prompt you to do so." ]
  , HH.p_ [ HH.text "4. Providing your personal data to others" ]
  , HH.p_ [ HH.text "4.1 We may disclose your personal data to any member of our group of companies (this means our subsidiaries, our ultimate holding company and all its subsidiaries) insofar as reasonably necessary for the purposes, and on the legal bases, set out in this policy." ]
  , HH.p_ [ HH.text "4.2 We may disclose your personal data to our insurers and/or professional advisers insofar as reasonably necessary for the purposes of obtaining or maintaining insurance coverage, managing risks, obtaining professional advice, or the establishment, exercise or defence of legal claims, whether in court proceedings or in an administrative or out-of-court procedure." ]
  , HH.p_ [ HH.text "4.3 We may disclose email addresses to our suppliers or subcontractors insofar as reasonably necessary for sending you our email newsletter, if you have requested it (you can inform us at any time if you no longer require the newsletter)." ]
  , HH.p_ [ HH.text "4.4 In addition to the specific disclosures of personal data set out in this Section 4, we may disclose your personal data where such disclosure is necessary for compliance with a legal obligation to which we are subject, or in order to protect your vital interests or the vital interests of another natural person. We may also disclose your personal data where such disclosure is necessary for the establishment, exercise or defence of legal claims, whether in court proceedings or in an administrative or out-of-court procedure." ]
  , HH.p_ [ HH.text "5. International transfers of your personal data" ]
  , HH.p_ [ HH.text "5.1 In this Section 5, we provide information about the circumstances in which your personal data may be transferred to countries outside the European Economic Area (EEA)." ]
  , HH.p_ [ HH.text "5.2 The hosting facilities for our website are situated in the US, Europe and the UK. The European Commission has made an \"adequacy decision\" with respect to the data protection laws of each of these countries. Transfers to each of these countries will be protected by appropriate safeguards, namely EU-U.S. Privacy Shield Framework." ]
  , HH.p_ [ HH.text "5.3 Mailing databases are situated in the US and Australia. The European Commission has made an \"adequacy decision\" with respect to the data protection laws of each of these countries. Transfers to each of these countries will be protected by appropriate safeguards, namely EU-U.S. Privacy Shield and Swiss-U.S. Privacy Shield." ]
  , HH.p_ [ HH.text "5.4 You acknowledge that personal data that you submit for publication through our website or services may be available, via the internet, around the world. We cannot prevent the use (or misuse) of such personal data by others." ]
  , HH.p_ [ HH.text "6. Retaining and deleting personal data" ]
  , HH.p_ [ HH.text "6.1 This Section 6 sets out our data retention policies and procedures, which are designed to help ensure that we comply with our legal obligations in relation to the retention and deletion of personal data." ]
  , HH.p_ [ HH.text "6.2 Personal data that we process for any purpose or purposes shall not be kept for longer than is necessary for that purpose or those purposes." ]
  , HH.p_ [ HH.text "6.3 We will retain and delete your personal data as follows:" ]
  , HH.p_ [ HH.text "(a) Profile data containing personal data will be retained for for the duration of your registration. If you wish for us to delete your TeamTavern account, then contact us." ]
  , HH.p_ [ HH.text "(b) Cookie data will be retained for no longer than 30 days." ]
  , HH.p_ [ HH.text "6.4 Notwithstanding the other provisions of this Section 6, we may retain your personal data where such retention is necessary for compliance with a legal obligation to which we are subject, or in order to protect your vital interests or the vital interests of another natural person." ]
  , HH.p_ [ HH.text "7. Amendments" ]
  , HH.p_ [ HH.text "7.1 We may update this policy from time to time by publishing a new version on our website." ]
  , HH.p_ [ HH.text "7.2 You should check this page occasionally to ensure you are happy with any changes to this policy." ]
  , HH.p_ [ HH.text "7.3 We may notify you of changes to this policy by email or through the private messaging system on our website." ]
  , HH.p_ [ HH.text "8. Your rights" ]
  , HH.p_ [ HH.text "8.1 In this Section 8, we have summarised the rights that you have under data protection law. Some of the rights are complex, and not all of the details have been included in our summaries. Accordingly, you should read the relevant laws and guidance from the regulatory authorities for a full explanation of these rights." ]
  , HH.p_ [ HH.text "8.2 Your principal rights under the data protection law are:" ]
  , HH.p_ [ HH.text "(a) the right to access;" ]
  , HH.p_ [ HH.text "(b) the right to rectification;" ]
  , HH.p_ [ HH.text "(c) the right to erasure;" ]
  , HH.p_ [ HH.text "(d) the right to restrict processing;" ]
  , HH.p_ [ HH.text "(e) the right to object to processing;" ]
  , HH.p_ [ HH.text "(f) the right to data portability;" ]
  , HH.p_ [ HH.text "(g) the right to complain to a supervisory authority; and" ]
  , HH.p_ [ HH.text "(h) the right to withdraw consent." ]
  , HH.p_ [ HH.text "8.3 You have the right to confirmation as to whether or not we process your personal data and, where we do, access to the personal data, together with certain additional information. That additional information includes details of the purposes of the processing, the categories of personal data concerned and the recipients of the personal data. Providing the rights and freedoms of others are not affected, we will supply to you a copy of your personal data. The first copy will be provided free of charge, but additional copies may be subject to a reasonable fee. Provision of such information will be subject to the supply of appropriate evidence of your identity (for this purpose, we will usually accept a photocopy of your passport certified by a solicitor or bank plus an original copy of a utility bill showing your current address)." ]
  , HH.p_ [ HH.text "8.4 You have the right to have any inaccurate personal data about you rectified and, taking into account the purposes of the processing, to have any incomplete personal data about you completed." ]
  , HH.p_ [ HH.text "8.5 In some circumstances you have the right to the erasure of your personal data without undue delay. Those circumstances include: the personal data are no longer necessary in relation to the purposes for which they were collected or otherwise processed; you withdraw consent to consent-based processing; you object to the processing under certain rules of applicable data protection law; the processing is for direct marketing purposes; and the personal data have been unlawfully processed. However, there are exclusions of the right to erasure. The general exclusions include where processing is necessary: for exercising the right of freedom of expression and information; for compliance with a legal obligation; or for the establishment, exercise or defence of legal claims." ]
  , HH.p_ [ HH.text "8.6 In some circumstances you have the right to restrict the processing of your personal data. Those circumstances are: you contest the accuracy of the personal data; processing is unlawful but you oppose erasure; we no longer need the personal data for the purposes of our processing, but you require personal data for the establishment, exercise or defence of legal claims; and you have objected to processing, pending the verification of that objection. Where processing has been restricted on this basis, we may continue to store your personal data. However, we will only otherwise process it: with your consent; for the establishment, exercise or defence of legal claims; for the protection of the rights of another natural or legal person; or for reasons of important public interest." ]
  , HH.p_ [ HH.text "8.7 You have the right to object to our processing of your personal data on grounds relating to your particular situation, but only to the extent that the legal basis for the processing is that the processing is necessary for: the performance of a task carried out in the public interest or in the exercise of any official authority vested in us; or the purposes of the legitimate interests pursued by us or by a third party. If you make such an objection, we will cease to process the personal information unless we can demonstrate compelling legitimate grounds for the processing which override your interests, rights and freedoms, or the processing is for the establishment, exercise or defence of legal claims." ]
  , HH.p_ [ HH.text "8.8 You have the right to object to our processing of your personal data for direct marketing purposes (including profiling for direct marketing purposes). If you make such an objection, we will cease to process your personal data for this purpose." ]
  , HH.p_ [ HH.text "8.9 If you consider that our processing of your personal information infringes data protection laws, you have a legal right to lodge a complaint with a supervisory authority responsible for data protection. You may do so in the EU member state of your habitual residence, your place of work or the place of the alleged infringement." ]
  , HH.p_ [ HH.text "8.10 To the extent that the legal basis for our processing of your personal information is consent, you have the right to withdraw that consent at any time. Withdrawal will not affect the lawfulness of processing before the withdrawal." ]
  , HH.p_ [ HH.text "8.11 You may exercise any of your rights in relation to your personal data by contacting us using the details outlined in section 13." ]
  , HH.p_ [ HH.text "9. About cookies" ]
  , HH.p_ [ HH.text "9.1 A cookie is a file containing an identifier (a string of letters and numbers) that is sent by a web server to a web browser and is stored by the browser. The identifier is then sent back to the server each time the browser requests a page from the server." ]
  , HH.p_ [ HH.text "9.2 Cookies may be either \"persistent\" cookies or \"session\" cookies: a persistent cookie will be stored by a web browser and will remain valid until its set expiry date, unless deleted by the user before the expiry date; a session cookie, on the other hand, will expire at the end of the user session, when the web browser is closed." ]
  , HH.p_ [ HH.text "9.3 Cookies do not typically contain any information that personally identifies a user, but personal information that we store about you may be linked to the information stored in and obtained from cookies." ]
  , HH.p_ [ HH.text "10. Cookies that we use" ]
  , HH.p_ [ HH.text "10.1 We use cookies for the following purposes:" ]
  , HH.p_ [ HH.text "(a) authentication - we use cookies to identify you when you visit our website and as you navigate our website;" ]
  , HH.p_ [ HH.text "(b) status - we use cookies to help us to determine if you are logged into our website;" ]
  , HH.p_ [ HH.text "(c) security - we use cookies as an element of the security measures used to protect user accounts, including preventing fraudulent use of login credentials, and to protect our website and services generally;" ]
  , HH.p_ [ HH.text "(d) advertising - we use cookies to help us to display advertisements that will be relevant to you (cookies used for this purpose are: Google AdSense, Google AdExchange, Avocet, PulsePoint, Teads, Skimlinks, Monetizer 101);" ]
  , HH.p_ [ HH.text "(e) analysis - we use cookies to help us to analyse the use and performance of our website and services (cookies used for this purpose are: Google Analytics, ComScore, Google Tag Manager, Crazy Egg, Facebook Audience, Google Analytics Audience);" ]
  , HH.p_ [ HH.text "(f) cookie consent - we use cookies to store your preferences in relation to the use of cookies more generally." ]
  , HH.p_ [ HH.text "11. Cookies used by our service providers" ]
  , HH.p_ [ HH.text "11.1 We use third party advertisements to support our site. Some of these advertisers may be served from our third party advertiser, you can view their privacy policy and cookie policy here." ]
  , HH.p_ [ HH.text "11.2 We use Google Analytics and Comscore to analyse the use of our website. Google Analytics and Comscore gather information about website use by means of cookies. The information gathered relating to our website is used to create reports about the use of our website. Google's privacy policy is available at: ", textAnchor_ "https://www.google.com/policies/privacy/" ". Comscore’s privacy policy is available at ", textAnchor_ "https://www.comscore.com/About-comScore/Privacy-Policy" "." ]
  , HH.p_ [ HH.text "12. Managing cookies" ]
  , HH.p_ [ HH.text "12.1 Most browsers allow you to refuse to accept cookies and to delete cookies. The methods for doing so vary from browser to browser, and from version to version. You can however obtain up-to-date information about blocking and deleting cookies via these links:" ]
  , HH.p_ [ HH.text "(a) ", textAnchor_ "https://support.google.com/chrome/answer/95647?hl=en" "(Chrome);" ]
  , HH.p_ [ HH.text "(b) ", textAnchor_ "https://support.mozilla.org/en-US/kb/enable-and-disable-cookies-website-preferences" "(Firefox);" ]
  , HH.p_ [ HH.text "(c) ", textAnchor_ "https://help.opera.com/en/latest/security-and-privacy/" "(Opera);" ]
  , HH.p_ [ HH.text "(d) ", textAnchor_ "https://support.microsoft.com/en-gb/help/17442/windows-internet-explorer-delete-manage-cookies" "(Internet Explorer);" ]
  , HH.p_ [ HH.text "(e) ", textAnchor_ "https://support.apple.com/kb/PH21411" "(Safari); and" ]
  , HH.p_ [ HH.text "(f) ", textAnchor_ "https://privacy.microsoft.com/en-us/windows-10-microsoft-edge-and-privacy" "(Edge)." ]
  , HH.p_ [ HH.text "12.2 Blocking all cookies will have a negative impact upon the usability of many websites." ]
  , HH.p_ [ HH.text "12.3 If you block cookies, you will not be able to use all the features on our website." ]
  , HH.p_ [ HH.text "12.4 You can manage your cookies ", HH.a [ HS.class_ "nn-cmp-show", HP.href "#" ] [ HH.text "here" ], HH.text "." ]
  ]
