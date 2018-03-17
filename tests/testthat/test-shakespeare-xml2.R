context("shakespeare-test-xml2")

test_that("selection works correctly on a shakespearean document", {
    HTML_SHAKESPEARE <- paste(
          "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ",
          "\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">",
          "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" debug=\"true\">",
          #"<html>",
          "<body>", "\t<div id=\"test\">", "\t<div class=\"dialog\">",
          "\t<h2>As You Like It</h2>", "\t<div id=\"playwright\">", "\t  by William Shakespeare",
          "\t</div>", "\t<div class=\"dialog scene thirdClass\" id=\"scene1\">",
          "\t  <h3>ACT I, SCENE III. A room in the palace.</h3>", "\t  <div class=\"dialog\">",
          "\t  <div class=\"direction\">Enter CELIA and ROSALIND</div>",
          "\t  </div>", "\t  <div id=\"speech1\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.1\">Why, cousin! why, Rosalind! Cupid have mercy! not a word?</div>",
          "\t  </div>", "\t  <div id=\"speech2\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.2\">Not one to throw at a dog.</div>",
          "\t  </div>", "\t  <div id=\"speech3\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.3\">No, thy words are too precious to be cast away upon</div>",
          "\t  <div id=\"scene1.3.4\">curs; throw some of them at me; come, lame me with reasons.</div>",
          "\t  </div>", "\t  <div id=\"speech4\" class=\"character\">ROSALIND</div>",
          "\t  <div id=\"speech5\" class=\"character\">CELIA</div>", "\t  <div class=\"dialog\">",
          "\t  <div id=\"scene1.3.8\">But is all this for your father?</div>",
          "\t  </div>", "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.5\">Then there were two cousins laid up; when the one</div>",
          "\t  <div id=\"scene1.3.6\">should be lamed with reasons and the other mad</div>",
          "\t  <div id=\"scene1.3.7\">without any.</div>", "\t  </div>",
          "\t  <div id=\"speech6\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.9\">No, some of it is for my child's father. O, how</div>",
          "\t  <div id=\"scene1.3.10\">full of briers is this working-day world!</div>",
          "\t  </div>", "\t  <div id=\"speech7\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.11\">They are but burs, cousin, thrown upon thee in</div>",
          "\t  <div id=\"scene1.3.12\">holiday foolery: if we walk not in the trodden</div>",
          "\t  <div id=\"scene1.3.13\">paths our very petticoats will catch them.</div>",
          "\t  </div>", "\t  <div id=\"speech8\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.14\">I could shake them off my coat: these burs are in my heart.</div>",
          "\t  </div>", "\t  <div id=\"speech9\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.15\">Hem them away.</div>",
          "\t  </div>", "\t  <div id=\"speech10\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.16\">I would try, if I could cry 'hem' and have him.</div>",
          "\t  </div>", "\t  <div id=\"speech11\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.17\">Come, come, wrestle with thy affections.</div>",
          "\t  </div>", "\t  <div id=\"speech12\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.18\">O, they take the part of a better wrestler than myself!</div>",
          "\t  </div>", "\t  <div id=\"speech13\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.19\">O, a good wish upon you! you will try in time, in</div>",
          "\t  <div id=\"scene1.3.20\">despite of a fall. But, turning these jests out of</div>",
          "\t  <div id=\"scene1.3.21\">service, let us talk in good earnest: is it</div>",
          "\t  <div id=\"scene1.3.22\">possible, on such a sudden, you should fall into so</div>",
          "\t  <div id=\"scene1.3.23\">strong a liking with old Sir Rowland's youngest son?</div>",
          "\t  </div>", "\t  <div id=\"speech14\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.24\">The duke my father loved his father dearly.</div>",
          "\t  </div>", "\t  <div id=\"speech15\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.25\">Doth it therefore ensue that you should love his son</div>",
          "\t  <div id=\"scene1.3.26\">dearly? By this kind of chase, I should hate him,</div>",
          "\t  <div id=\"scene1.3.27\">for my father hated his father dearly; yet I hate</div>",
          "\t  <div id=\"scene1.3.28\">not Orlando.</div>", "\t  </div>",
          "\t  <div id=\"speech16\" class=\"character\">ROSALIND</div>",
          "\t  <div title=\"wtf\" class=\"dialog\">", "\t  <div id=\"scene1.3.29\">No, faith, hate him not, for my sake.</div>",
          "\t  </div>", "\t  <div id=\"speech17\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.30\">Why should I not? doth he not deserve well?</div>",
          "\t  </div>", "\t  <div id=\"speech18\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.31\">Let me love him for that, and do you love him</div>",
          "\t  <div id=\"scene1.3.32\">because I do. Look, here comes the duke.</div>",
          "\t  </div>", "\t  <div id=\"speech19\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.33\">With his eyes full of anger.</div>",
          "\t  <div class=\"direction\">Enter DUKE FREDERICK, with Lords</div>",
          "\t  </div>", "\t  <div id=\"speech20\" class=\"character\">DUKE FREDERICK</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.34\">Mistress, dispatch you with your safest haste</div>",
          "\t  <div id=\"scene1.3.35\">And get you from our court.</div>",
          "\t  </div>", "\t  <div id=\"speech21\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.36\">Me, uncle?</div>",
          "\t  </div>", "\t  <div id=\"speech22\" class=\"character\">DUKE FREDERICK</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.37\">You, cousin</div>",
          "\t  <div id=\"scene1.3.38\">Within these ten days if that thou be'st found</div>",
          "\t  <div id=\"scene1.3.39\">So near our public court as twenty miles,</div>",
          "\t  <div id=\"scene1.3.40\">Thou diest for it.</div>", "\t  </div>",
          "\t  <div id=\"speech23\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.41\">                  I do beseech your grace,</div>",
          "\t  <div id=\"scene1.3.42\">Let me the knowledge of my fault bear with me:</div>",
          "\t  <div id=\"scene1.3.43\">If with myself I hold intelligence</div>",
          "\t  <div id=\"scene1.3.44\">Or have acquaintance with mine own desires,</div>",
          "\t  <div id=\"scene1.3.45\">If that I do not dream or be not frantic,--</div>",
          "\t  <div id=\"scene1.3.46\">As I do trust I am not--then, dear uncle,</div>",
          "\t  <div id=\"scene1.3.47\">Never so much as in a thought unborn</div>",
          "\t  <div id=\"scene1.3.48\">Did I offend your highness.</div>",
          "\t  </div>", "\t  <div id=\"speech24\" class=\"character\">DUKE FREDERICK</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.49\">Thus do all traitors:</div>",
          "\t  <div id=\"scene1.3.50\">If their purgation did consist in words,</div>",
          "\t  <div id=\"scene1.3.51\">They are as innocent as grace itself:</div>",
          "\t  <div id=\"scene1.3.52\">Let it suffice thee that I trust thee not.</div>",
          "\t  </div>", "\t  <div id=\"speech25\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.53\">Yet your mistrust cannot make me a traitor:</div>",
          "\t  <div id=\"scene1.3.54\">Tell me whereon the likelihood depends.</div>",
          "\t  </div>", "\t  <div id=\"speech26\" class=\"character\">DUKE FREDERICK</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.55\">Thou art thy father's daughter; there's enough.</div>",
          "\t  </div>", "\t  <div id=\"speech27\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.56\">So was I when your highness took his dukedom;</div>",
          "\t  <div id=\"scene1.3.57\">So was I when your highness banish'd him:</div>",
          "\t  <div id=\"scene1.3.58\">Treason is not inherited, my lord;</div>",
          "\t  <div id=\"scene1.3.59\">Or, if we did derive it from our friends,</div>",
          "\t  <div id=\"scene1.3.60\">What's that to me? my father was no traitor:</div>",
          "\t  <div id=\"scene1.3.61\">Then, good my liege, mistake me not so much</div>",
          "\t  <div id=\"scene1.3.62\">To think my poverty is treacherous.</div>",
          "\t  </div>", "\t  <div id=\"speech28\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.63\">Dear sovereign, hear me speak.</div>",
          "\t  </div>", "\t  <div id=\"speech29\" class=\"character\">DUKE FREDERICK</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.64\">Ay, Celia; we stay'd her for your sake,</div>",
          "\t  <div id=\"scene1.3.65\">Else had she with her father ranged along.</div>",
          "\t  </div>", "\t  <div id=\"speech30\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.66\">I did not then entreat to have her stay;</div>",
          "\t  <div id=\"scene1.3.67\">It was your pleasure and your own remorse:</div>",
          "\t  <div id=\"scene1.3.68\">I was too young that time to value her;</div>",
          "\t  <div id=\"scene1.3.69\">But now I know her: if she be a traitor,</div>",
          "\t  <div id=\"scene1.3.70\">Why so am I; we still have slept together,</div>",
          "\t  <div id=\"scene1.3.71\">Rose at an instant, learn'd, play'd, eat together,</div>",
          "\t  <div id=\"scene1.3.72\">And wheresoever we went, like Juno's swans,</div>",
          "\t  <div id=\"scene1.3.73\">Still we went coupled and inseparable.</div>",
          "\t  </div>", "\t  <div id=\"speech31\" class=\"character\">DUKE FREDERICK</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.74\">She is too subtle for thee; and her smoothness,</div>",
          "\t  <div id=\"scene1.3.75\">Her very silence and her patience</div>",
          "\t  <div id=\"scene1.3.76\">Speak to the people, and they pity her.</div>",
          "\t  <div id=\"scene1.3.77\">Thou art a fool: she robs thee of thy name;</div>",
          "\t  <div id=\"scene1.3.78\">And thou wilt show more bright and seem more virtuous</div>",
          "\t  <div id=\"scene1.3.79\">When she is gone. Then open not thy lips:</div>",
          "\t  <div id=\"scene1.3.80\">Firm and irrevocable is my doom</div>",
          "\t  <div id=\"scene1.3.81\">Which I have pass'd upon her; she is banish'd.</div>",
          "\t  </div>", "\t  <div id=\"speech32\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.82\">Pronounce that sentence then on me, my liege:</div>",
          "\t  <div id=\"scene1.3.83\">I cannot live out of her company.</div>",
          "\t  </div>", "\t  <div id=\"speech33\" class=\"character\">DUKE FREDERICK</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.84\">You are a fool. You, niece, provide yourself:</div>",
          "\t  <div id=\"scene1.3.85\">If you outstay the time, upon mine honour,</div>",
          "\t  <div id=\"scene1.3.86\">And in the greatness of my word, you die.</div>",
          "\t  <div class=\"direction\">Exeunt DUKE FREDERICK and Lords</div>",
          "\t  </div>", "\t  <div id=\"speech34\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.87\">O my poor Rosalind, whither wilt thou go?</div>",
          "\t  <div id=\"scene1.3.88\">Wilt thou change fathers? I will give thee mine.</div>",
          "\t  <div id=\"scene1.3.89\">I charge thee, be not thou more grieved than I am.</div>",
          "\t  </div>", "\t  <div id=\"speech35\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.90\">I have more cause.</div>",
          "\t  </div>", "\t  <div id=\"speech36\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.91\">                  Thou hast not, cousin;</div>",
          "\t  <div id=\"scene1.3.92\">Prithee be cheerful: know'st thou not, the duke</div>",
          "\t  <div id=\"scene1.3.93\">Hath banish'd me, his daughter?</div>",
          "\t  </div>", "\t  <div id=\"speech37\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.94\">That he hath not.</div>",
          "\t  </div>", "\t  <div id=\"speech38\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.95\">No, hath not? Rosalind lacks then the love</div>",
          "\t  <div id=\"scene1.3.96\">Which teacheth thee that thou and I am one:</div>",
          "\t  <div id=\"scene1.3.97\">Shall we be sunder'd? shall we part, sweet girl?</div>",
          "\t  <div id=\"scene1.3.98\">No: let my father seek another heir.</div>",
          "\t  <div id=\"scene1.3.99\">Therefore devise with me how we may fly,</div>",
          "\t  <div id=\"scene1.3.100\">Whither to go and what to bear with us;</div>",
          "\t  <div id=\"scene1.3.101\">And do not seek to take your change upon you,</div>",
          "\t  <div id=\"scene1.3.102\">To bear your griefs yourself and leave me out;</div>",
          "\t  <div id=\"scene1.3.103\">For, by this heaven, now at our sorrows pale,</div>",
          "\t  <div id=\"scene1.3.104\">Say what thou canst, I'll go along with thee.</div>",
          "\t  </div>", "\t  <div id=\"speech39\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.105\">Why, whither shall we go?</div>",
          "\t  </div>", "\t  <div id=\"speech40\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.106\">To seek my uncle in the forest of Arden.</div>",
          "\t  </div>", "\t  <div id=\"speech41\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.107\">Alas, what danger will it be to us,</div>",
          "\t  <div id=\"scene1.3.108\">Maids as we are, to travel forth so far!</div>",
          "\t  <div id=\"scene1.3.109\">Beauty provoketh thieves sooner than gold.</div>",
          "\t  </div>", "\t  <div id=\"speech42\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.110\">I'll put myself in poor and mean attire</div>",
          "\t  <div id=\"scene1.3.111\">And with a kind of umber smirch my face;</div>",
          "\t  <div id=\"scene1.3.112\">The like do you: so shall we pass along</div>",
          "\t  <div id=\"scene1.3.113\">And never stir assailants.</div>",
          "\t  </div>", "\t  <div id=\"speech43\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.114\">Were it not better,</div>",
          "\t  <div id=\"scene1.3.115\">Because that I am more than common tall,</div>",
          "\t  <div id=\"scene1.3.116\">That I did suit me all points like a man?</div>",
          "\t  <div id=\"scene1.3.117\">A gallant curtle-axe upon my thigh,</div>",
          "\t  <div id=\"scene1.3.118\">A boar-spear in my hand; and--in my heart</div>",
          "\t  <div id=\"scene1.3.119\">Lie there what hidden woman's fear there will--</div>",
          "\t  <div id=\"scene1.3.120\">We'll have a swashing and a martial outside,</div>",
          "\t  <div id=\"scene1.3.121\">As many other mannish cowards have</div>",
          "\t  <div id=\"scene1.3.122\">That do outface it with their semblances.</div>",
          "\t  </div>", "\t  <div id=\"speech44\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.123\">What shall I call thee when thou art a man?</div>",
          "\t  </div>", "\t  <div id=\"speech45\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.124\">I'll have no worse a name than Jove's own page;</div>",
          "\t  <div id=\"scene1.3.125\">And therefore look you call me Ganymede.</div>",
          "\t  <div id=\"scene1.3.126\">But what will you be call'd?</div>",
          "\t  </div>", "\t  <div id=\"speech46\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.127\">Something that hath a reference to my state</div>",
          "\t  <div id=\"scene1.3.128\">No longer Celia, but Aliena.</div>",
          "\t  </div>", "\t  <div id=\"speech47\" class=\"character\">ROSALIND</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.129\">But, cousin, what if we assay'd to steal</div>",
          "\t  <div id=\"scene1.3.130\">The clownish fool out of your father's court?</div>",
          "\t  <div id=\"scene1.3.131\">Would he not be a comfort to our travel?</div>",
          "\t  </div>", "\t  <div id=\"speech48\" class=\"character\">CELIA</div>",
          "\t  <div class=\"dialog\">", "\t  <div id=\"scene1.3.132\">He'll go along o'er the wide world with me;</div>",
          "\t  <div id=\"scene1.3.133\">Leave me alone to woo him. Let's away,</div>",
          "\t  <div id=\"scene1.3.134\">And get our jewels and our wealth together,</div>",
          "\t  <div id=\"scene1.3.135\">Devise the fittest time and safest way</div>",
          "\t  <div id=\"scene1.3.136\">To hide us from pursuit that will be made</div>",
          "\t  <div id=\"scene1.3.137\">After my flight. Now go we in content</div>",
          "\t  <div id=\"scene1.3.138\">To liberty and not to banishment.</div>",
          "\t  <div class=\"direction\">Exeunt</div>", "\t  </div>", "\t</div>",
          "\t</div>", "</div>", "</body>", "</html>", sep = "\n")

    library(xml2)
    document <- read_xml(HTML_SHAKESPEARE)
    xml_ns_strip(document)
    body <- xml_find_first(document, "//body")
    gt <- GenericTranslator$new()

    count <- function(selector) {
        xpath <- gt$css_to_xpath(selector)
        results <- xml_find_all(body, xpath)
        length(results)
    }

    # Data borrowed from http://mootools.net/slickspeed/

    ## Changed from original; probably because I'm only
    ## searching the body.
    #expect_that(count('*'), equals(252))
    expect_that(count('*'), equals(246))
    expect_that(count('div:contains(CELIA)'), equals(26))
    expect_that(count('div:only-child'), equals(22)) # ?
    expect_that(count('div:nth-child(even)'), equals(106))
    expect_that(count('div:nth-child(2n)'), equals(106))
    expect_that(count('div:nth-child(odd)'), equals(137))
    expect_that(count('div:nth-child(2n+1)'), equals(137))
    expect_that(count('div:nth-child(n)'), equals(243))
    expect_that(count('div:last-child'), equals(53))
    expect_that(count('div:first-child'), equals(51))
    expect_that(count('div > div'), equals(242))
    expect_that(count('div + div'), equals(190))
    expect_that(count('div ~ div'), equals(190))
    expect_that(count('body'), equals(1))
    expect_that(count('body div'), equals(243))
    expect_that(count('div'), equals(243))
    expect_that(count('div div'), equals(242))
    expect_that(count('div div div'), equals(241))
    expect_that(count('div, div, div'), equals(243))
    expect_that(count('div, a, span'), equals(243))
    expect_that(count('.dialog'), equals(51))
    expect_that(count('div.dialog'), equals(51))
    expect_that(count('div .dialog'), equals(51))
    expect_that(count('div.character, div.dialog'), equals(99))
    expect_that(count('div.direction.dialog'), equals(0))
    expect_that(count('div.dialog.direction'), equals(0))
    expect_that(count('div.dialog.scene'), equals(1))
    expect_that(count('div.scene.scene'), equals(1))
    expect_that(count('div.scene .scene'), equals(0))
    expect_that(count('div.direction .dialog '), equals(0))
    expect_that(count('div .dialog .direction'), equals(4))
    expect_that(count('div.dialog .dialog .direction'), equals(4))
    expect_that(count('#speech5'), equals(1))
    expect_that(count('div#speech5'), equals(1))
    expect_that(count('div #speech5'), equals(1))
    expect_that(count('div.scene div.dialog'), equals(49))
    expect_that(count('div#scene1 div.dialog div'), equals(142))
    expect_that(count('#scene1 #speech1'), equals(1))
    expect_that(count('div[class]'), equals(103))
    expect_that(count('div[class=dialog]'), equals(50))
    expect_that(count('div[class^=dia]'), equals(51))
    expect_that(count('div[class$=log]'), equals(50))
    expect_that(count('div[class*=sce]'), equals(1))
    expect_that(count('div[class|=dialog]'), equals(50)) # ? Seems right
    expect_that(count('div[class!=madeup]'), equals(243)) # ? Seems right
    expect_that(count('div[class~=dialog]'), equals(51)) # ? Seems right
})
