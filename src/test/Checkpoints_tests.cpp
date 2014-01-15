//
// Unit tests for block-chain checkpoints
//
#include <boost/assign/list_of.hpp> // for 'map_list_of()'
#include <boost/test/unit_test.hpp>
#include <boost/foreach.hpp>

#include "../checkpoints.h"
#include "../util.h"

using namespace std;

BOOST_AUTO_TEST_SUITE(Checkpoints_tests)

BOOST_AUTO_TEST_CASE(sanity)
{
    uint256 p56808 = uint256("0x30803fb9567e24b91f101135716c756a8f6876a5a40bc625861e70a2aa2f91e9");
    uint256 p56603 = uint256("0x307a73808a2db07ce34024ee833eec8df27b61984ceee686e6dc6418c08603e0");
    BOOST_CHECK(Checkpoints::CheckBlock(56808, p56808));
    BOOST_CHECK(Checkpoints::CheckBlock(56603, p56603));

    
    // Wrong hashes at checkpoints should fail:
    BOOST_CHECK(!Checkpoints::CheckBlock(56808, p56603));
    BOOST_CHECK(!Checkpoints::CheckBlock(56603, p56808));

    // ... but any hash not at a checkpoint should succeed:
    BOOST_CHECK(Checkpoints::CheckBlock(56808+1, p56603));
    BOOST_CHECK(Checkpoints::CheckBlock(56603+1, p56808));

    BOOST_CHECK(Checkpoints::GetTotalBlocksEstimate() >= 56603);
}    

BOOST_AUTO_TEST_SUITE_END()
